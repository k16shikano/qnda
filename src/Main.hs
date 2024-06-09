{-# LANGUAGE Arrows, FlexibleContexts #-}

module Main where
 
import Text.XML.HXT.Core hiding (xshow)
import Text.XML.HXT.Arrow.XmlArrow hiding (xshow)
import Text.XML.HXT.DOM.ShowXml (xshow)

import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.Map as Map
import Data.Hashable (hash)

import Control.Monad as M hiding (when)

import System.Environment (getArgs)
import Data.Time.Clock.POSIX (getPOSIXTime)

import qualified Codec.Archive.Zip as ZIP

import EPUB.HtmlReader (readHtml, getTextFromNode, resolveHierarchy) 
import EPUB.AuxReader (readAux)
import EPUB.MathReader (configEquations)
import EPUB.ImageReader (imagePathToResourceName)
import EPUB.MetaInformation (mkCoverpage, genOkuzuke, getMetaInfo, mkOpf, mkContainer, getIsbn)
import EPUB.Counter (readLabelInfo)
import EPUB.Toc (mkTocpage, mkNcx, mkHeaderCnt)

import Options.Applicative
import Data.Semigroup ((<>))

import qualified Debug.Trace as DT (trace)

-- main

main :: IO ()
main = epub =<< execParser opts
  where
    opts = info (options <**> helper)
      (fullDesc
       <> progDesc "Make EPUB from HTML"
       <> header "qnda - an epub maker" )

data CmdOpt = CmdOpt {
  math :: String,
  image  :: String,
  file :: FilePath
}

options :: Parser CmdOpt
options = CmdOpt
          <$> strOption
              ( long "math" <> short 'm' <> value "png" <> metavar "MATH-TYPE" <> help "Type of mathmatical equations")
          <*> strOption
              ( long "image" <> short 'i' <> value "png" <> metavar "IMG-TYPE" <> help "Type of images")
          <*> strArgument
              ( help "input book.xhtml file" <> metavar "FILE" <> action "file" )

epub :: CmdOpt -> IO ()
epub (CmdOpt mathtype imgtype book) = do
  epochtime <- floor `fmap` getPOSIXTime  
  let mkEntry path content = ZIP.toEntry path epochtime content
  let cssfiles = ["css/epub.css", "css/fonts.css"]
  let cssdir = "css"
  let coverimg = "cover.jpg"
  let coverfilename = "titlepage.xhtml"
  let tocpagefile = "toc.xhtml"
  let okuzukepagefile = "okuzuke.xhtml"
  let ncxfile = "toc.ncx"
  let auxfile = "book.aux" -- if you will
  let fontsdir = "fonts"
  let outputFileName = "output.epub"
  
  isbn <- getIsbn book
  
  coverfile <- mkCoverpage coverimg
  coverimgEntry <- ZIP.readEntry [] coverimg
  let coverfileEntry = mkEntry coverfilename $ fromString . xshow $ coverfile
  
  okuzuke <- genOkuzuke book
  let okuzukeEntry = mkEntry okuzukepagefile $ fromString . xshow $ okuzuke
  
  -- Qualifing each file with an information whether it's to be a chapter, frontmatter, or appendix.
  -- The information must resist in the "book" file.  
  hOnes <- runX (
    readDocument [withValidate no] book
    >>>
    (multi (ifA (hasName "include")
            ((getChildren >>> getText)
             &&&
             ((getChildren >>> getText >>> arr (++".xhtml"))
              >>> arrIO hOneCounter >>> unlistA))
            (none))))
  let hOnesInFiles = Map.fromList hOnes
  htmls <- runX (
    readDocument [withValidate no] book
    >>>
    fromSLA (0,"frontmatter", "")
    (deep (ifA (hasName "include")
            (nextState . (\(filename, (numOfh1, (cur,_,_))) (n,x,_) 
                          -> ( (if cur==0 then numOfh1 else n+numOfh1)
                             , (if numOfh1 == 0 && x == "chapter"
                                then "part"
                                else (if x == "part" then "chapter" else x))
                             , filename)) 
                           $< ((getChildren >>> getText >>> arr (++".xhtml"))
                               &&&
                               ((\f -> case Map.lookup f hOnesInFiles of
                                    Just i -> constA i
                                    Nothing -> constA 0)
                                $< (getChildren >>> getText))
                                &&&
                               getState))
            (ifA (hasName "mainmatter")
             (constA (0, "chapter", "") >>> setState)
             (ifA (hasName "appendix")
              (constA (0, "appendix", "") >>> setState) 
              (ifA (hasName "backmatter")
               (constA (0, "backmatter", "") >>> setState)
               (none))))))
    )
  let shift ns = zipWith (-) ns (0 : map (\x -> x - 1) (zipWith (-) (drop 1 ns) ns))
  let tempHtmlFiles = filter (\(_,_,f) -> f/="") htmls
      sequentNumbers = map (\(n,_,_) -> n) tempHtmlFiles
      hOneTypes = map (\(_,t,_) -> t) tempHtmlFiles
      htmlFilenames = map (\(_,_,f) -> f) tempHtmlFiles
      htmlFiles' = zip3 sequentNumbers hOneTypes htmlFilenames
      htmlFiles = seqPartNumber 0 htmlFiles'
  
  -- Retrieving reference-ids from all the html files and letting each id be pair with the filename,
  -- in order to make distinct ids in the epub file.  
  labelsAndFiles <-
    mapM (\s -> do 
             links <- runX (readDocument [withIndent no
                                         ,withRemoveWS yes
                                         ,withValidate no
                                         ] s
                            >>>
                            multi (ifA (hasName "ref" <+> hasName "a" <+> hasName "li" <+> hasName "pref" <+> hasName "pageref" 
                                        <+> (hasName "aside" >>> hasAttrValue "epub:type" (=="footnote")))
                                   (constA "")
                                   (getAttrValue "label" <+> getAttrValue "id")))
--             return $ map (flip (,) s) $ filter (/="") links
             return $ map (\link -> (link, (resolveHierarchy s)++s)) $ filter (/="") links
         ) htmlFilenames
  let internalLinkLabels = Map.fromList $ concat labelsAndFiles
  
--  print internalLinkLabels
  
  (mathImages, mathImageEntries') <- configEquations mathtype htmlFilenames
  mathImageEntries <- mathImageEntries'

  labelmap <- readLabelInfo htmlFiles
  htmlData <- mapM (\(n,t,f) -> readHtml f internalLinkLabels mathtype labelmap n t) htmlFiles
  let htmlEntries = zipWith (\(n,t,f) d -> mkEntry f d) htmlFiles htmlData

  images <- liftM concat $ mapM imagePathToResourceName htmlFilenames
  imageEntries <- mapM (ZIP.readEntry []) images
  
  metadata <- getMetaInfo book
  opf <- mkOpf metadata htmlFiles images mathImages (coverimg, coverfilename) tocpagefile okuzukepagefile ncxfile cssfiles
  let opfEntry = mkEntry "content.opf" $ fromString . xshow $ opf

  -- extract all the header elements to generate toc and ncx
  headers <- 
    mapM (\(n,t,f) ->
             runX (readDocument [withValidate no] f 
                   >>> 
                   (fromSLA (n-1)
                    (multi
                     (ifA (hasName "h0")
                      (getName &&& (getTextFromNode labelmap) &&& constA f &&&
                       (constA $ (mkHeaderCnt "part" n)))
                      (ifA (hasName "h1" <+> hasName "appendix")
                       (getName &&& (getTextFromNode labelmap) &&& constA f &&&
                        (ifA (hasAttrValue "nonum" (=="yes"))
                         (constA $ mkHeaderCnt "other" n)
                         (constA . (mkHeaderCnt t) $< (nextState (+1)))))
                       (ifA (hasName "h2")
                        (getName &&& (getTextFromNode labelmap) &&& constA f &&&
                         (ifA (hasAttrValue "nonum" (=="yes")) 
                          (constA $ mkHeaderCnt "other" n) 
                          (constA $ mkHeaderCnt t n)))
                        (none)))))))
         ) htmlFiles
  
  ncx <- mkNcx (concat isbn) (concat headers)
  let ncxEntry = mkEntry ncxfile $ fromString . xshow $ ncx

  tocpage <- mkTocpage $ concat headers
  let tocpageEntry = mkEntry tocpagefile $ fromString . xshow $ tocpage
  
  let mimetypeEntry = mkEntry "mimetype" $ fromString "application/epub+zip"
  
  stylesheetEntry <- ZIP.readEntry [ZIP.OptRecursive] cssdir
  fontsEntry <- ZIP.readEntry [ZIP.OptRecursive] fontsdir
      
  container <- mkContainer
  let containerEntry = mkEntry "META-INF/container.xml" $ fromString . xshow $ container
  
  -- gather all the entries around!
  let archive' =  foldr ZIP.addEntryToArchive ZIP.emptyArchive (containerEntry : ncxEntry : opfEntry : stylesheetEntry :
                                                                coverfileEntry : coverimgEntry : tocpageEntry : okuzukeEntry : 
                                                                (htmlEntries ++ imageEntries ++ mathImageEntries))

  archive'' <- ZIP.addFilesToArchive [ZIP.OptRecursive] archive' [ fontsdir, cssdir ]
  
  -- ensure mimetype to be the first entry
  let archive = ZIP.addEntryToArchive mimetypeEntry archive''
  
  B.writeFile outputFileName $ ZIP.fromArchive archive
  
  return ()

hOneCounter :: String -> IO [Int]
hOneCounter file = do
  c <- runX (
    readDocument [withValidate no] file 
    >>>
    listA (this //> multi (hasName "h1" >>> neg (hasAttr "nonum")))
    >>>
    arr length)
  return c

seqPartNumber :: Int -> [(Int, String, FilePath)] -> [(Int, String, FilePath)]
seqPartNumber i ((_, "part", fp):xs) = (i+1, "part", fp):(seqPartNumber (i+1) xs)
seqPartNumber i (x:xs) = x : (seqPartNumber i xs)
seqPartNumber i [] = []

