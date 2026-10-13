{-# LANGUAGE FlexibleContexts #-}

module EPUB.Bibliography where

import Text.XML.HXT.Core hiding (xshow)
import Text.XML.HXT.Arrow.XmlArrow hiding (xshow)
import Text.XML.HXT.DOM.ShowXml (xshow)

import qualified Data.Map as Map
import Data.List (nubBy, isPrefixOf)

import qualified Debug.Trace as DT (trace)
import qualified Data.Maybe

type CslEntryId = String
type CslEntry = XmlTree
type CslEntryWithId = (CslEntryId, CslEntry, FilePath)

-- Extract csl-entry divs from a file using arrow
extractCslEntries :: FilePath -> IO [CslEntryWithId]
extractCslEntries filename = runX (
  readDocument [withValidate no] filename
  >>>
  (this //> hasAttrValue "id" (=="refs"))
  >>>
  getChildren
  >>>
  multi (ifA (hasAttrValue "class" (=="csl-entry"))
          ((getAttrValue "id" &&& (this &&& constA filename))
           >>>
           arr (\(id, (entry, f)) -> (removeRefPrefix id, entry, f)))
          none))
  where
    removeRefPrefix id = if "ref-" `isPrefixOf` id then drop 4 id else id

-- Merge entries, removing duplicates by id
mergeCslEntries :: [CslEntryWithId] -> [CslEntryWithId]
mergeCslEntries = nubBy (\(id1, _, _) (id2, _, _) -> id1 == id2)

-- Create mapping from entry id to new number
mkCitationMapping :: [CslEntryWithId] -> Map.Map String Int
mkCitationMapping entries =
  Map.fromList $ zipWith (\(id, _, _) num -> (id, num)) entries [1..]

-- Helper function to replace all occurrences
replaceAll :: String -> String -> String -> String
replaceAll old new = go
  where
    go [] = []
    go s@(x:xs)
      | old `isPrefixOf` s = new ++ go (drop (length old) s)
      | otherwise = x : go xs

-- Escape XML special characters (& must be first to avoid double-escaping)
escapeXml :: String -> String
escapeXml =
  replaceAll "'" "&#39;"
  . replaceAll "\"" "&quot;"
  . replaceAll ">" "&gt;"
  . replaceAll "<" "&lt;"
  . replaceAll "&" "&amp;"

-- Convert LaTeX escape sequences and HTML special characters to HTML entities
escapeLaTeX :: String -> String
escapeLaTeX = escapeXml . replaceAll "\\&" "&"

-- Process a single entry with updated numbering
processEntry :: (CslEntryId, CslEntry, Int) -> IO XmlTree
processEntry (id, entry, num) = do
  (processed:_) <- runX (
    constA entry
    >>>
    processBottomUp (
      updateCslLeftMargin num `when` hasAttrValue "class" (=="csl-left-margin")
      >>>
      updateCslEntryId id `when` hasAttrValue "class" (=="csl-entry")
      >>>
      (processAttrl (changeAttrValue (escapeXml . replaceAll "\\_" "_") `when` hasName "href")) `when` hasName "a"
      >>>
      (txt . escapeLaTeX $< getText) `when` isText
      )
    )
  return processed

-- Generate bibliography.xhtml
mkBibliographyPage :: [CslEntryWithId] -> IO ([XmlTree], Map.Map String Int)
mkBibliographyPage entries = do
  let numberedEntries = zipWith (\(id, entry, _) num -> (id, entry, num)) entries [1..]
  let citationMap = mkCitationMapping entries

  processedEntries <- mapM processEntry numberedEntries

  bibliography <- runX (
    spi "xml" "version=\"1.0\" encoding=\"UTF-8\""
    <+>
    (eelem "html"
     += sattr "xmlns" "http://www.w3.org/1999/xhtml"
     += sattr "xml:lang" "ja-JP"
     += (eelem "head"
         += (eelem "title"
             += txt "Bibliography")
         += (eelem "link"
             += sattr "rel" "stylesheet"
             += sattr "type" "text/css"
             += sattr "href" "css/epub.css")
         += (eelem "link"
             += sattr "rel" "stylesheet"
             += sattr "type" "text/css"
             += sattr "href" "css/fonts.css"))
     += (eelem "body"
         += (eelem "h1"
             += txt "参考文献")
         += (eelem "div"
             += sattr "id" "refs"
             += sattr "class" "references csl-bib-body"
             += sattr "data-entry-spacing" "0"
             += sattr "role" "list"
             += catA (map constA processedEntries)))))

  return (bibliography, citationMap)

-- Update csl-left-margin with new number using arrow
updateCslLeftMargin :: (ArrowXml a) => Int -> a XmlTree XmlTree
updateCslLeftMargin num =
  eelem "div"
  += sattr "class" "csl-left-margin"
  += txt ("[" ++ show num ++ "] ")

-- Update csl-entry id to preserve original id (with ref- prefix)
updateCslEntryId :: (ArrowXml a) => String -> a XmlTree XmlTree
updateCslEntryId oldId =
  processAttrl (
    changeAttrValue (const ("ref-" ++ oldId)) `when` hasName "id"
  )

-- Update citation spans with new numbers and links using arrow
updateCitations :: (ArrowXml a) => Map.Map String Int -> String -> a XmlTree XmlTree
updateCitations citationMap bibliographyFile =
  processBottomUp (
    (mkCitationLink citationMap bibliographyFile $< getAttrValue "data-cites") `when`
      (hasName "span" >>> hasAttrValue "class" (=="citation"))
  )
  >>>
  processBottomUp (
    none `when` (hasName "div" >>> hasAttrValue "id" (=="refs"))
  )

-- Create citation link element
mkCitationLink citationMap bibliographyFile citesId =
  catA $ map mkSingleLink $ words citesId
  where
    mkSingleLink singleId =
      eelem "a"
      += sattr "href" labelfile
      += sattr "class" "citation"
      += txt ("[" ++ show newNum ++ "]")
      where
        newNum = Data.Maybe.fromMaybe 0 (Map.lookup singleId citationMap)
        entryId = "ref-" ++ singleId
        labelfile = bibliographyFile ++ "#" ++ entryId

