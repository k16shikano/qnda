{-# LANGUAGE Arrows, FlexibleContexts #-}

module Toc where

import Text.XML.HXT.Core hiding (xshow)
import Data.Hashable ( hash )
import Control.Monad.State as S hiding (when)
import Data.Char (chr)

--import qualified Debug.Trace as DT (trace)

type HeaderTag  = String           -- h1, h2, and so on.
type HeaderText = String           -- text of header
type HeaderCnt  = (String, String) -- (header counter string, header type)
type InTocText  = String           -- header prefix text appearing in toc
type Header     = (HeaderTag, (HeaderText, (FilePath, HeaderCnt)))
type HeaderList = [((Header, InTocText), Int)]

mkNcx :: String -> [Header] -> IO [XmlTree]
mkNcx isbn headers = runX (
  spi "xml" "version=\"1.0\" encoding=\"UTF-8\""
  <+>
  (eelem "ncx"
   += sattr "version" "2005-1"
   += sattr "xmlns" "http://www.daisy.org/z3986/2005/ncx/"
   += (eelem "head"
       += (eelem "meta" += sattr "name" "dtb:uid" += sattr "content" ("urn:isbn:"++isbn))
       += (eelem "meta" += sattr "name" "dtb:depth" += sattr "content" "2")
       += (eelem "meta" += sattr "name" "dtb:totalPageCount" += sattr "content" "0")
       += (eelem "meta" += sattr "name" "dtb:maxPageNumber" += sattr "content" "0"))
   += (eelem "docTitle"
       += eelem "text")
   += (eelem "navMap"
       += (mkNavTree $ zip (zip headers (evalState (chapSectNum headers) (0,0,[]))) [1..]))))

chapSectNum :: [Header] -> S.State (Int,Int,[InTocText]) [InTocText]
chapSectNum [] = do
  (_,_,result) <- get
  return $ reverse result
chapSectNum ((t, (h, (f, c))) : cs)  = do
  (chap, sect, result) <- get
  let h1cnt = h1Prefix c
      h2cnt = h2Prefix c sect
  put $ 
    case t of
      "h1"  -> (chap, 0, h1cnt : result)
      "h2"  -> (chap, sect+1, h2cnt : result)
      _     -> (chap, sect, ("" : result))
  chapSectNum cs

mkHeaderCnt :: String -> Int -> HeaderCnt
mkHeaderCnt "chapter" n = (show n, "chapter")
mkHeaderCnt "appendix" n = ([chr $ 64+n], "appendix")
mkHeaderCnt _ n    = ("", "other")

h1Prefix :: HeaderCnt -> String
h1Prefix (c, "chapter") = "Chapter: "++c++" "
h1Prefix (c, "appendix") = "Appendix"++c++" "
h1Prefix (c, "other") = ""

h2Prefix :: HeaderCnt -> Int -> String
h2Prefix (c, "chapter") n = c++"."++(show $ n+1)++" "
h2Prefix (c, "appendix") n = c++"."++(show $ n+1)++" "
h2Prefix (c, "other") n = ""

takeChildren prod = takeWhile (prod . fst . fst . fst)
dropChildren prod = dropWhile (prod . fst . fst . fst)

mkTreeFromHeaderList :: (ArrowList a, ArrowXml a) =>
                        (HeaderTag -> FilePath ->  InTocText -> Int -> (String -> Bool) -> HeaderList -> a b XmlTree)
                        -> (HeaderTag -> FilePath -> InTocText -> Int -> a b XmlTree)
                        -> HeaderList
                        -> a b XmlTree
mkTreeFromHeaderList whenRoot whenNode [] = none
mkTreeFromHeaderList whenRoot whenNode c@(((("appendix",(h,(f,cnt))),cs),n) : ((("h2",(_,_)),_),_) : _) = -- when head is "appendix"
  whenRoot ("h1"++h) f cs n (\h -> (h=="h2") || (h=="h3")) (tail c)
mkTreeFromHeaderList whenRoot whenNode c@(((("h1",(h,(f,cnt))),cs),n) : ((("h1",(_,_)),_),_) : _) = -- when head is h1 and the rest are h1
  whenRoot ("h1"++h) f cs n (\h -> (h=="h2") || (h=="h3")) (tail c)
mkTreeFromHeaderList whenRoot whenNode c@(((("h1",(h,(f,cnt))),cs),n) : ((("h2",(_,_)),_),_) : _) = -- when head is h1 and the rest are h2 or h3
  whenRoot ("h1"++h) f cs n (\h -> (h=="h2") || (h=="h3")) (tail c)
mkTreeFromHeaderList whenRoot whenNode c@(((("h2",(h,(f,cnt))),cs),n) : ((("h3",(_,_)),_),_) : _) = -- when head is h1 and the rest are h3 but h2
  whenRoot ("h1"++h) f cs n (=="h3") (tail c) 
mkTreeFromHeaderList whenRoot whenNode c = -- when head is not h1, so is h2 or h3
  let typeOfHead = fst $ fst $ fst $ head c
  in  (catA (map (\(((_,(h,(f,cnt))),cs),n) -> whenNode ("h2"++h) f cs n) $ takeChildren (==typeOfHead) c))
      <+> (mkTreeFromHeaderList whenRoot whenNode $ dropChildren (==typeOfHead) c)


---------
-- NCX --
---------

mkNavTree :: (ArrowList a, ArrowXml a) => HeaderList -> a b XmlTree
mkNavTree = mkTreeFromHeaderList mkNavRoot mkNavNode

mkNavRoot :: (ArrowList a, ArrowXml a) => HeaderText -> FilePath -> InTocText -> Int -> (String -> Bool) -> HeaderList -> a b XmlTree
mkNavRoot headertext filename chapsec number children rest = 
  (mkNavNode headertext filename chapsec number
   += (mkNavTree $ takeChildren children rest))
  <+> (mkNavTree $ dropChildren children rest)

mkNavNode :: (ArrowList a, ArrowXml a) => HeaderText -> FilePath -> InTocText -> Int -> a b XmlTree
mkNavNode headertext filename chapsec number =
  eelem "navPoint"
  += sattr "id" ("navPoint-"++(show number))
  += sattr "playOrder" (show number)
  += (eelem "navLabel"
      += (eelem "text" += (txt $ (chapsec++(drop 2 headertext)))))
  += (eelem "content"
      += (sattr "src" $ filename++"#"++"name"++(show $ hash headertext)))

---------
-- TOC --
---------

mkTocpage headers = runX (
  spi "xml" "version=\"1.0\" encoding=\"UTF-8\""
  <+>
  (eelem "html"
   += sattr "xmlns" "http://www.w3.org/1999/xhtml"
   += sattr "xml:lang" "ja-JP"
   += (eelem "head"
       += (eelem "title"
           += txt "Table of Contents")
       += (eelem "link" 
           += sattr "rel" "stylesheet" 
           += sattr "type" "text/css"
           += sattr "href" "css/epub.css")
       += (eelem "link" 
           += sattr "rel" "stylesheet" 
           += sattr "type" "text/css"
           += sattr "href" "css/fonts.css"))
   += (eelem "body"
       += (eelem "nav" += sattr "xmlns:epub" "http://www.idpf.org/2007/ops" += sattr "epub:type" "toc"
           += (eelem "h1" += txt "Table of Contents")
           += (eelem "ol"
               += (mkTocTree $ zip (zip headers (evalState (chapSectNum headers) (0,0,[]))) [1..]))))))

mkTocTree :: (ArrowList a, ArrowXml a) => HeaderList -> a b XmlTree
mkTocTree = mkTreeFromHeaderList mkTocRoot mkTocNode

mkTocRoot :: (ArrowList a, ArrowXml a) => HeaderText -> FilePath -> InTocText -> Int -> (String -> Bool) -> HeaderList -> a b XmlTree
mkTocRoot headertext filename chapsec number children rest = 
  (mkTocNode headertext filename chapsec number
   += (eelem "ol"
       += (mkTocTree $ takeChildren children rest)))
  <+> (mkTocTree $ dropChildren children rest)

mkTocNode :: (ArrowList a, ArrowXml a) => HeaderText -> FilePath -> InTocText -> Int -> a b XmlTree
mkTocNode headertext filename chapsec number =
  (eelem "li"
   += (eelem "a"
       += (sattr "href" $ filename++"#"++"name"++(show $ hash headertext))
       += (txt $ chapsec)
       += (txt $ drop 2 headertext)))
