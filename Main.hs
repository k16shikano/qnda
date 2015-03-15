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

import HtmlReader (readHtml, getTextFromNode)
import AuxReader (readAux)
import MathReader (mathElemToResourceName, genImageFromEqString)
import ImageReader (imagePathToResourceName)
import MetaInformation (mkCoverpage, genOkuzuke, getMetaInfo, mkOpf, mkContainer, getIsbn)
import Counter (readLabelInfo)
import Toc (mkTocpage, mkNcx)

-- import qualified Debug.Trace as DT (trace)

-- main
main :: IO ()
main = do
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
  let outputFileName = "ymnwcp.epub"
  
  [book] <- getArgs
  isbn <- getIsbn book
  
  coverfile <- mkCoverpage coverimg
  coverimgEntry <- ZIP.readEntry [] coverimg
  let coverfileEntry = mkEntry coverfilename $ fromString . xshow $ coverfile
  
  okuzuke <- genOkuzuke book
  let okuzukeEntry = mkEntry okuzukepagefile $ fromString . xshow $ okuzuke
  
  -- Qualifing each file with an information whether it's to be a chapter, frontmatter, or appendix.
  -- The information must resist in the "book" file.  
  htmls <- runX (
    readDocument [withValidate no] book
    >>>
    fromSLA (0,"frontmatter", "")
    (multi (ifA (hasName "include")
            (nextState . (\(filename, numOfh1) (n,x,_) -> (n+numOfh1, x, filename)) 
                           $< ((getChildren >>> getText >>> arr (++".html"))
                               &&& (listA (multi (hasName "h1")) >>> arr length)))
            (ifA (hasName "mainmatter")
             (constA (0, "chapter", "") >>> setState)
             (ifA (hasName "appendix") 
              (constA (0, "appendix", "") >>> setState) 
              (none)))))
    )
  let htmlFiles = filter (\(n,t,f) -> f/="") htmls
  let htmlFilenames = map (\(n,t,f) -> f) htmlFiles

  -- Retrieving reference-ids from all the html files and letting each id be pair with the filename,
  -- in order to make distinct ids in the epub file.  
  labelsAndFiles <-
    mapM (\s -> do 
             links <- runX (readDocument [withIndent no
                                         ,withRemoveWS yes
                                         ,withValidate no
                                         ] s
                            >>>
                            multi (ifA (hasName "ref" <+> hasName "pref" <+> hasName "pageref")
                                   (constA "")
                                   (getAttrValue "label")))
             return $ map (flip (,) s) $ filter (/="") links
         ) htmlFilenames
  let internalLinkLabels = Map.fromList $ concat labelsAndFiles

  maths <- mapM mathElemToResourceName htmlFilenames
  mapM_ (\(imagepath, equation) -> genImageFromEqString imagepath equation) $ concat maths
  let mathSnipets = Map.fromList $ concat maths
  let mathimages = map fst $ concat maths
  mathImageEntries <- mapM (ZIP.readEntry []) mathimages

  labelmap <- readLabelInfo htmlFiles
  htmlData <- mapM (\(n,t,f) -> readHtml f internalLinkLabels mathSnipets labelmap n t) htmlFiles
  let htmlEntries = zipWith (\(n,t,f) d -> mkEntry f d) htmlFiles htmlData

  images <- liftM concat $ mapM imagePathToResourceName htmlFilenames
  imageEntries <- mapM (ZIP.readEntry []) images
  
  metadata <- getMetaInfo book
  opf <- mkOpf metadata htmlFiles images mathimages (coverimg, coverfilename) tocpagefile okuzukepagefile ncxfile cssfiles
  let opfEntry = mkEntry "content.opf" $ fromString . xshow $ opf

  -- extract all the header elements to generate toc and ncx
  headers <- 
    mapM (\f -> do
             runX (readDocument [withValidate no] f 
                   >>> 
                   (multi
                    (ifA ((hasName "h1" <+> hasName "h2" <+> hasName "appendix")) 
                     (this) (none))
                    >>>
                    ((getName) &&& (getTextFromNode labelmap) &&& constA f)))
         ) htmlFilenames

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
  
