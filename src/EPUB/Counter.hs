{-# LANGUAGE Arrows, FlexibleContexts #-}

module EPUB.Counter where

import Text.XML.HXT.Core hiding (xshow)

import Data.Hashable (hash)
import Data.Char (chr, isAlpha, isAlphaNum)
import Data.String.Utils 
import qualified Data.Map as Map

-- import qualified Debug.Trace as DT (trace)

type CounterState = ( Int -- chapter
                    , Int -- section
                    , Int -- subsection
                    , Int -- figure
                    , Int -- table
                    , Int -- list
                    )

mkPartHead :: String -> String
mkPartHead n = "Part " <> n <> ". "

mkChapterHead :: String  -> String
mkChapterHead n = "Chapter " <> n <> ". "

mkSectionHead :: String  -> String  -> String
mkSectionHead sn cn = "§" <> sn <> "." <> cn <> ". "

mkAppendixHead :: String  -> String
mkAppendixHead n = "Appendix " <> n <> ". "

figCaptionHeader = "Fig-"
tableCaptionHeader = "Table-"

getChapter :: CounterState -> String
getChapter (n,_,_,_,_,_) = show $ n-1

nextChapter :: SLA CounterState b CounterState
nextChapter = nextState update
    where update (n,_,_,_,_,_) = (n+1,0,0,0,0,0)

getAppendix :: CounterState -> String
getAppendix (n,_,_,_,_,_) = [chr (64+n)]

genChapterHeader :: String -> SLA CounterState XmlTree XmlTree
genChapterHeader "part" = (txt . (\c -> mkPartHead (getChapter c))) $< nextChapter
genChapterHeader "chapter" = (txt . (\c -> mkChapterHead (getChapter c))) $< nextChapter
genChapterHeader "appendix" = (txt . (\c -> mkAppendixHead (getAppendix c))) $< nextChapter
genChapterHeader _ = txt ""

seekChapterLabel :: String -> SLA CounterState XmlTree XmlTree
seekChapterLabel headertype = seekLabel "h1" $ genChapterHeader headertype

seekPartLabel :: String -> SLA CounterState XmlTree XmlTree
seekPartLabel headertype = seekLabel "h0" $ genChapterHeader headertype


getSection :: CounterState -> String
getSection (_,n,_,_,_,_) = show n

nextSection :: SLA CounterState XmlTree CounterState
nextSection = nextState update
    where update (c,n,_,f,t,l) = (c,n+1,0,f,t,l)

genSectionHeader :: String -> SLA CounterState XmlTree XmlTree
genSectionHeader "chapter" = (txt . (\c -> (getChapter c) <> "." <> (getSection c) <> "")) $< nextSection
genSectionHeader "appendix" = (txt . (\c -> (getAppendix c) <> "." <> (getSection c) <> "")) $< nextSection
genSectionHeader t = txt ""

seekSectionLabel :: String -> SLA CounterState XmlTree XmlTree
seekSectionLabel headertype = seekLabel "h2" $ genSectionHeader headertype


getSubSection :: CounterState -> String
getSubSection (_,_,n,_,_,_) = show n

nextSubSection :: SLA CounterState XmlTree CounterState
nextSubSection = nextState update
    where update (c,s,n,f,t,l) = (c,s,n+1,f,t,l)

genSubSectionHeader :: String -> SLA CounterState XmlTree XmlTree
genSubSectionHeader "chapter" = (txt . (\c -> (getChapter c) <> "." <> (getSection c) <> "." <> (getSubSection c) <> "")) $< nextSubSection
genSubSectionHeader "appendix" = (txt . (\c -> (getAppendix c) <> "." <> (getSection c) <> (getSubSection c) <> "")) $< nextSubSection
genSubSectionHeader t = txt ""

seekSubSectionLabel :: String -> SLA CounterState XmlTree XmlTree
seekSubSectionLabel headertype = seekLabel "h3" $ genSubSectionHeader headertype


getFigure :: CounterState -> String
getFigure (_,_,_,n,_,_) = show n

nextFigure :: SLA CounterState XmlTree CounterState
nextFigure = nextState update
    where update (c,s,ss,n,t,l) = (c,s,ss,n+1,t,l)

genFigureHeader :: String -> SLA CounterState XmlTree XmlTree
genFigureHeader "chapter" = (txt . (\c -> figCaptionHeader <> (getChapter c) <> "." <> (getFigure c) <> "")) $< nextFigure
genFigureHeader "appendix" = (txt . (\c -> figCaptionHeader <> (getAppendix c) <> "." <> (getFigure c) <> "")) $< nextFigure
genFigureHeader t = txt ""

seekFigureLabel :: String -> SLA CounterState XmlTree XmlTree
seekFigureLabel headertype = seekLabel "figcaption" $ genFigureHeader headertype


getTable :: CounterState -> String
getTable (_,_,_,_,n,_) = show n

nextTable :: SLA CounterState XmlTree CounterState
nextTable = nextState update
    where update (c,s,ss,f,n,l) = (c,s,ss,f,n+1,l)

genTableHeader :: String -> SLA CounterState XmlTree XmlTree
genTableHeader "chapter" = (txt . (\c -> tableCaptionHeader <> (getChapter c) <> "." <> (getTable c) <> "")) $< nextTable
genTableHeader "appendix" = (txt . (\c -> tableCaptionHeader <> (getAppendix c) <> "." <> (getTable c) <> "")) $< nextTable
genTableHeader t = txt ""

seekTableLabel :: String -> SLA CounterState XmlTree XmlTree
seekTableLabel headertype = seekLabel "caption" $ genTableHeader headertype


getList :: CounterState -> String
getList (_,_,_,_,_,n) = show n

nextList :: SLA CounterState XmlTree CounterState
nextList = nextState update
    where update (c,s,ss,f,t,n) = (c,s,ss,f,t,n+1)

genListHeader :: String -> SLA CounterState XmlTree XmlTree
genListHeader "chapter" = (txt . (\c -> figCaptionHeader <> (getChapter c) <> "." <> (getList c) <> "")) $< nextList
genListHeader "appendix" = (txt . (\c -> figCaptionHeader <> (getAppendix c) <> "." <> (getList c) <> "")) $< nextList
genListHeader t = txt ""

seekListLabel :: String -> SLA CounterState XmlTree XmlTree
seekListLabel headertype = seekLabel "caption" $ genListHeader headertype


-----

readLabelInfo :: [(Int, String, String)]
              -> IO (Map.Map String  -- label
                            ( String -- text of header number
                            , String -- text of header body
                            ))
readLabelInfo htmlFiles = do
  headers <- mapM 
             (\(n, t, html) ->
                runX (readDocument [withValidate no] html
                      >>>
                      fromSLA (n,0,0,0,0,0) (genHeaders t)
                      >>>
                      (deep
                       (hasName "entry")
                        >>>
                        deepest (ifA (hasName "label") (getChildren >>> getText) (none))
                         &&&
                        deepest (ifA (hasName "hhead") (getChildren >>> getText) (none))
                         &&&
                        deepest (ifA (hasName "hbody") (getChildren >>> getText) (none))))
             ) htmlFiles
  return $ Map.fromList $ concat headers

genHeaders t = processBottomUp (
  (seekPartLabel t) `when` (hasName "h0" >>> neg (hasAttr "nonum"))
  >>>
  (seekChapterLabel t) `when` (hasName "h1" >>> neg (hasAttr "nonum"))
  >>>
  (seekSectionLabel t) `when` (hasName "h2" >>> neg (hasAttr "nonum"))
  >>>
  (seekSubSectionLabel t) `when` (hasName "h3" >>> neg (hasAttr "nonum"))
  >>>
  (processChildren $ seekFigureLabel t) `when` (hasName "figure")
  >>>
  (processChildren $ seekTableLabel t) `when` (hasName "table"))

seekLabel :: String -> SLA CounterState XmlTree XmlTree -> SLA CounterState XmlTree XmlTree
seekLabel whose how = deepest (hasName whose)
                      >>>
                      (replaceChildren
                       (eelem "entry"
                        += (eelem "label" 
                            += choiceA [ (hasAttr "id") :->
                                         (txt . idTrim $< getAttrValue "id")
                                       , (hasAttr "label") :->
                                         (txt . idTrim $< getAttrValue "label")
                                       , this :->
                                         (txt . mkLabel $< (getChildren >>> getText))
                                      ])                          
                        += (eelem "hhead" += how)
                        += (eelem "hbody" += (txt $< deepest (getChildren >>> getText)))))
    where mkLabel s = "name" <> (show $ hash s)
   
idTrim :: String -> String
idTrim = idTrim' False
  where idTrim' startchar str = case str of 
          '&':_ -> idTrim' False $ join ";" $ tail $ split ";" str 
          a:rest -> case startchar of
            True  -> if isAlpha a then a : idTrim' False rest else idTrim' False rest
            False -> if isAlphaNum a || ('_' == a) || ('-' == a) || ('.' == a) || (':' == a)
                     then a : idTrim' False rest else idTrim' False rest
          [] -> []
