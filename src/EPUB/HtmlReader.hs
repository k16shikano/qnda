{-# LANGUAGE Arrows, FlexibleContexts #-}

module EPUB.HtmlReader (readHtml, getTextFromNode, resolveHierarchy) where
 
import Text.XML.HXT.Core hiding (xshow)
import Data.Hashable ( hash )
import qualified Data.String.Utils as String (split)

import qualified Data.Map as Map

import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.UTF8 ( fromString )

import qualified System.FilePath.Posix as FP

import EPUB.MathReader
import EPUB.ImageReader
import EPUB.Counter


-- import qualified Debug.Trace as DT (trace)

mkHtml :: FilePath -> (Map.Map String String) -> (Map.Map String String) -> (Map.Map String (String, String))
          -> Int -> String -> IO B.ByteString
mkHtml f labels mathsnipets label n t = do 
  let body = readHtml f labels mathsnipets label n t
  body 

readHtml ::    FilePath 
            -> (Map.Map String String)           -- Labels for internal links 
            -> (Map.Map String String)           -- Equations to be converted into images
            -> (Map.Map String (String, String)) -- Title header by label
            -> Int                               -- chapter number
            -> String                            -- whether chapter or appendix
            -> IO B.ByteString
readHtml filename labels mathSnipets label n t = do

  body <-
    runX (
      readDocument [withIndent yes
                   ,withRemoveWS no
                   ,withValidate no
                   ] filename
      >>>
      processBottomUp (
                
        -- Inline elements        
        (this >>> processAttrl ((setAttrName (mkName "id") >>> changeAttrValue idTrim) `when` hasAttr "name"
                                <+>
                                (setAttrName (mkName "href") >>> changeAttrValue idTrim) `when` hasAttr "href")) 
        `when` hasName "a"
        >>>
        (mkExternalLink $< (this >>> getChildren)) `when` hasName "url"
        >>>
        (mkInternalLinkPage labels $< (getAttrValue "label")) `when` hasName "pageref"
        >>>
        (mkInternalLink labels label $< (getAttrValue "label")) `when` hasName "ref"
        >>>
        (eelem "b" += (this >>> getChildren)) `when` (hasName "kw" <+> hasName "em" <+> hasName "b")
        >>>
        (eelem "u" += (this >>> getChildren)) `when` hasName "u"
        >>>
        (eelem "i" += (this >>> getChildren)) `when` hasName "it"
        >>>
        (eelem "ruby" 
         += (this >>> getChildren)
         += (eelem "rt" += (txt $< (this >>> getAttrValue "yomi"))))
        `when` hasName "ruby"
        >>>
        (eelem "tt" += (this >>> getChildren))
        `when` (hasName "filename" <+> hasName "tt")
        >>>
       
        
        -- Footnotes
        (eelem "span"
         += sattr "class" "footnote" 
         += (getChildren >>> (ifA (hasName "title") ((txt . (++"： ")) $< (this /> getText)) this)))
         `when` hasName "footnote"
        >>>
        
        
        -- Block elements
        (eelem "p" 
         += sattr "class" "para" 
         += (this >>> getChildren)
         <+>
         (ifA (this >>> hasAttr "label") (eelem "a" += (sattr "id" . idTrim $< getAttrValue "label")) (none)))
        `when` hasName "p"
        >>>
        (eelem "pre" 
         += sattr "class" "code" 
         += (this >>> getChildren >>> 
             (ifA (isText)
              ((\t -> foldl (\l r -> ifA l (l <+> eelem "br" <+> r) (r)) none $ map txt (String.split "\n" t)) $< getText)
              (this))))
        `when` hasName "codelist"
        >>>
        (this >>> 
         ((\c -> case c of
              "" -> this      
              _  -> addAttr "style" $ "background-color:"++c++";") $< getAttrValue "bgcolor")
         >>> removeAttr "bgcolor" >>> removeAttr "cline")
        `when` hasName "tr"
        >>>
        (ifA (this //> ((hasText (\s -> (not $ all (`elem` " \n\t") s))) <+> hasName "img"))
         (((\(a,w) -> case (a,w) of
               ("","") -> this
               ("",w)  -> addAttr "style" $ "width:"++w++";"
               (a,"")  -> addAttr "style" $ "align:"++a++";"
               (a,w)   -> addAttr "style" $ "width:"++w++";align:"++a++";")
           $<$ ((getAttrValue "align") &&& (getAttrValue "width")))
          >>> removeAttr "align" >>> removeAttr "width")
         (none)) `when` (hasName "td" <+> hasName "th")
        >>>
        (eelem "div"
         += sattr "class" "column"
         += (getChildren
             >>>
             (ifA (hasName "title")
              ((putLabel . (++"column") $< getTextFromNode label) <+> (eelem "h4" += getChildren))
              this)))
        `when` hasName "column"
        >>>
        (eelem "div"
         += (putLabel . ("h2"++) $< (this /> getText))
         += sattr "class" "parthead"
         += (txt (mkPartHead (show n))
             <+> getChildren))
        `when` hasName "h0"
      

        
        
        -- Misc
        >>>
        ((eelem "div" += sattr "style" "float:right;" += (txt $< getAttrValue "date") )
         <+> 
         (eelem "div" += sattr "style" "float:right;clear:both;" += (this >>> getChildren))
         <+>
         (eelem "br"))
        `when` hasName "signature"
        >>>
        (eelem "span" += sattr "class" "authorname" += (this >>> getChildren)) `when` (hasName "aboutauthor" >>> hasName "name")
        >>>
        (eelem "span" += sattr "class" "authorcontact" += (this >>> getChildren)) `when` (hasName "aboutauthor" >>> hasName "contact")
        >>>
        (eelem "div" += sattr "class" "aboutauthor" += (this >>> getChildren)) `when` hasName "aboutauthor"
        >>>
        (eelem "div" += sattr "class" "authordesc" += (this >>> getChildren)) `when` hasName "authordesc"
        >>>
        (eelem "div" += sattr "class" "chapterlead" += (this >>> getChildren)) `when` hasName "chapterlead"
        )
      
      >>>
      replaceChildren 
      (spi "xml" "version=\"1.0\" encoding=\"UTF-8\""
       <+>(eelem "html" 
           += sattr "xmlns" "http://www.w3.org/1999/xhtml"
           += (eelem "head" 
               += (eelem "link" 
                   += sattr "rel" "stylesheet" 
                   += sattr "type" "text/css"
                   += sattr "href" (resolveHierarchy filename ++ "css/epub.css"))
               += (eelem "link"
                   += sattr "rel" "stylesheet" 
                   += sattr "type" "text/css"
                   += sattr "href" (resolveHierarchy filename ++ "css/fonts.css"))
               += (this /> hasName "title")
               += (eelem "meta" += sattr "charset" "utf-8"))
           += (deep 
               (ifA (hasName "body") 
                (this >>> 
                 setElemName (mkName "body") >>> 
                 removeAttr "id")
                (none))
               += deep 
                   (ifA (hasName "footnotes") 
                    (this >>> 
                     (setElemName (mkName "div") += sattr "class" "footnotes")) 
                    (none)))))
      
      >>>
      genChapSecSubsec n t label
      
      >>>
      mathElem filename
      >>>
      imgElem filename
            
      
      >>>
      processTopDown (
        (ifA (hasName "include")
         (none)
         (this)))
        
      >>>
      writeDocumentToString [ withIndent no
                            , withRemoveWS no
                            , withOutputXHTML
                            , withXmlPi yes
                            ]
      )
  
  return $ fromString $ concat body

resolveHierarchy s = 
  (concat . replicate (length $ (filter (/=".") $ FP.splitPath (FP.takeDirectory s)))) "../"

getTextFromNode :: (ArrowXml a) => (Map.Map String (String, String)) -> a XmlTree String
getTextFromNode  label = 
  (listA (getChildren >>> multi ((replaceLabel label $< (getAttrValue "label")) `when` hasName "ref") >>> getText))
  >>> arr concat

putLabel :: (ArrowXml a) => String -> a XmlTree XmlTree
putLabel t =
  ((ifA (hasAttr "label") (eelem "a" += (sattr "id" . idTrim $< getAttrValue "label")) (none))
   <+>
   (eelem "a" += (sattr "id" . idTrim $ (mkLabel t))))
  where mkLabel s = "name"++(show $ hash s)

mkExternalLink body = eelem "a"
                      += (sattr "href" $< (constA body >>> getText))
                      += constA body

mkInternalLinkPage linkfiles label = 
  eelem "a"
  += sattr "href" labelfile
  += txt "[ref within the book]"
  where 
    labelfile = case Map.lookup label linkfiles of
      Just filename -> filename++"#"++(idTrim label)
      Nothing -> (idTrim label)

mkInternalLink linkfiles map label = 
  eelem "a"
  += sattr "href" labelfile
  += labelinfo
  where 
    labelfile = case Map.lookup label linkfiles of
      Just filename -> filename++"#"++(idTrim label)
      Nothing -> (idTrim label)
    labelinfo = replaceLabel map label
  
replaceLabel map label = 
  txt labelinfo
  where 
    labelinfo = case Map.lookup label map of
      Just (counter, title) -> mkLinkenTitle label counter title
      Nothing -> "????"
    mkLinkenTitle label counter title = case take 3 label of
      "par" -> counter
      "cha" -> counter
      "chp" -> counter
      "sec" -> counter
      "fig" -> "" ++ counter
      "tbl" -> "" ++ counter
      "tou" -> counter
      "Wir" -> counter
      "mac" -> counter
      _ -> "[reference]"

ftnMark :: (ArrowXml a) => a XmlTree XmlTree
ftnMark = fromSLA 0 $
  processBottomUp $ 
  (mkFtnMark $< (nextState (+1) >>> arr show))
  `when` (hasName "footnote")

mkFtnMark :: (ArrowXml a) => String -> a XmlTree XmlTree
mkFtnMark n = eelem "a"
              += sattr "href" ("#ftn"++n)
              += (eelem "sup"
                  += txt ("†"++n))
              <+> (eelem "a"
                  += (sattr "id" . idTrim $ "ntf"++n))

ftnText :: (ArrowXml a) => a XmlTree XmlTree
ftnText = fromSLA 0 $
  processBottomUp $ 
  (mkFtnText $<< (nextState (+1) >>> arr show) &&& this)
  `when` (hasName "footnote")

mkFtnText :: (ArrowXml a) => String -> XmlTree -> a XmlTree XmlTree
mkFtnText n t = -- eelem "mbp:pagebreak" -- trick for kindle
                -- <+> 
                (eelem "div" 
                 += sattr "class" "footnote"
                 += sattr "style" "page-break-before: always;"
                 += eelem "a"
                 += (sattr "id" . idTrim $ "ftn"++n)
                 += (eelem "span" += txt ("†"++n) )
                 <+> (eelem "div"
                      += (constA t >>> getChildren))
                 <+> (eelem "a" += sattr "href" ("#ntf"++n) += charRef 8617))


genChapSecSubsec n t label = 
  fromSLA ((n,0,0,0,0,0) :: CounterState) (
    processBottomUp (
       ((eelem "h1"
         += (putLabel . ("h1"++) $< (this >>> getTextFromNode label))
         += (ifA (hasAttr "nonum")
             (getChildren)
             (case t of 
                "chapter" -> (txt . (\c -> (mkChapterHead $ getChapter c))) $< nextChapter
                "appendix" -> (txt . (\c -> (mkAppendixHead $ getAppendix c))) $< nextChapter
                _ -> txt "")
             <+> (txt $< (this >>> getTextFromNode label))))
        `when` (hasName "h1" <+> hasName "preface" <+> hasName "appendix"))
       >>>
       ((eelem "h2"
         += (putLabel . ("h2"++) $< (this >>> getTextFromNode label))
         += (ifA (hasAttr "nonum")
             (getChildren)
             (case t of 
                "chapter" -> (txt . (\c -> (getChapter c)++"."++(getSection c)++": ")) $< nextSection
                "appendix" -> (txt . (\c -> (getAppendix c)++"."++(getSection c)++": ")) $< nextSection
                _ -> (txt "")
              <+> (txt $< (this //> getText)))))
        `when` hasName "h2")
       >>>
       ((eelem "h3"
         += (putLabel . ("h3"++) $< (this >>> getTextFromNode label))
         += (ifA (hasAttr "nonum")
             (getChildren)
             (case t of
                "chapter" -> (txt . (\c -> (getChapter c)++"."++(getSection c)++"."++(getSubSection c)++": ")) $< nextSubSection
                "appendix" -> (txt . (\c -> (getAppendix c)++"."++(getSection c)++"."++(getSubSection c)++": ")) $< nextSubSection
                _ -> txt ""
              <+> getChildren)))
        `when` hasName "h3")

       >>>
       ((eelem "figure"
         +=
         (this //> choiceA 
          [ hasName "figcaption" :-> 
            (eelem "figcaption" 
             += txt figCaptionHeader
             += (case t of
                    "chapter" -> (txt . getChapter) $< getState
                    "appendix" -> (txt . getAppendix) $< getState
                    _ -> txt "")
             += txt "."
             += ((txt . getFigure) $< nextFigure)
             += txt "："
             += getChildren
             += (ifA (hasAttr "label") (eelem "a" += (sattr "id" . idTrim $< getAttrValue "label")) (none)))
          , hasName "p" :-> this
          , hasName "pre" :-> this
          , hasName "img" :-> this
          , this :-> this
          ]))
        `when` hasName "figure")

       >>>
       ((eelem "table"
         +=
         (this /> choiceA 
          [ hasName "caption" :->
            (eelem "caption" 
             += txt tableCaptionHeader
             += (case t of
                   "chapter" -> (txt . getChapter) $< getState
                   "appendix" -> (txt . getAppendix) $< getState
                   _ -> txt "")
             += txt "."
             += ((txt . getTable) $< nextTable) 
             += txt ": " 
             += getChildren
             += (ifA (hasAttr "label") (eelem "a" += (sattr "id" . idTrim $< getAttrValue "label")) (none)))
          , this :-> this
          ]))
        `when` hasName "table")))


