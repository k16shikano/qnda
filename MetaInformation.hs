{-# LANGUAGE Arrows, FlexibleContexts #-}

module MetaInformation where

import Text.XML.HXT.Core hiding (xshow)

import Data.Time.Clock (getCurrentTime)
-- import System.Locale (defaultTimeLocale)
import Data.Time.Format (formatTime, defaultTimeLocale)

--import qualified Debug.Trace as DT (trace)

getIsbn book = do
  runX (
    readDocument [withValidate no] book
    >>>
    (multi $ hasName "isbn" /> getText)
    )

genOkuzuke book = do
  runX (
    readDocument [withValidate no] book
    >>>
    (multi 
    (ifA (hasName "bookinfo")
     (mkOkuzuke $<<
      (((multi $ hasName "booktitle" >>> listA (getChildren >>> getText) >>> arrL ((:[]).concat))
        &&& (multi $ hasName "isbn" /> getText)
        &&& (multi $ hasName "copyright" /> getText)
        &&& (multi $ hasName "copyright" >>> getAttrValue "year")
        &&& (listA (multi $ hasName "printing") >>> arr reverse >>> arr head >>> getAttrValue "date")
        &&& (listA (multi $ hasName "printing") >>> arr reverse >>> arr head /> getText))
       &&& listA (multi $ hasName "authors" /> hasName "name" >>> this &&& getAttrValue "role" >>> arr authorRole)))
     (none))))

authorRole (name, "author") = (name, "aut")
authorRole (name, "translator") = (name, "trl")

mkOkuzuke (title, (isbn, (rights, (year, (date, version))))) authors =
  spi "xml" "version=\"1.0\" encoding=\"UTF-8\""
  <+>
  (eelem "html"
   += sattr "xmlns:epub" "http://www.idpf.org/2007/ops"
   += sattr "xmlns" "http://www.w3.org/1999/xhtml"
   += sattr "xml:lang" "ja"
   += (eelem "head" += (eelem "title" += txt title))
   += (eelem "body"
       += eelem "hr"
       += (eelem "p" += (txt title))
       += (eelem "p" += txt date += txt " " += txt version)
       += (eelem "div" += sattr "class" "author"
           += catA (map (\(name,role) -> eelem "p" += (constA name >>> getChildren))
                    authors))
       += (eelem "p" += entityRef "#x00A9" += txt rights += (eelem "i" += txt year))
       += (eelem "p" += txt "ISBN " += txt isbn)
       += eelem "hr")
   )

mkCoverpage coverimg = runX (
  eelem "html"
  += sattr "xmlns:epub" "http://www.idpf.org/2007/ops"
  += sattr "xmlns" "http://www.w3.org/1999/xhtml"
  += (eelem "head" 
      += (eelem "title" += txt "cover"))
  += (eelem "body"
      += (eelem "div" 
          += sattr "class" "coverpage" 
          += sattr "style" "height:95%;width:auto;"
          += (eelem "img"
              += sattr "src" "cover.jpg"
              += sattr "alt" "cover"
              += sattr "id" "coverimage"))))

mkContainer = runX (
  eelem "container" 
  += sattr "version" "1.0"
  += sattr "xmlns" "urn:oasis:names:tc:opendocument:xmlns:container"
  += (eelem "rootfiles"
      += (eelem "rootfile" 
          += sattr "full-path" "content.opf" 
          += sattr "media-type" "application/oebps-package+xml")))
              
---------
-- opf --
---------

getMetaInfo book = do
  utctime <- getCurrentTime >>= return . formatTime defaultTimeLocale "%Y-%m-%dT%TZ"
  runX (
    readDocument [withValidate no] book
    >>>
    listA (multi
           (ifA (hasName "bookinfo")
            (mkOpfMetaData $<<
             (((multi $ hasName "booktitle" >>> listA (getChildren >>> getText) >>> arrL ((:[]).concat))
               &&& (multi $ hasName "isbn" /> getText)
               &&& (multi $ hasName "copyright" /> getText)
               &&& (constA utctime))
              &&& listA (multi $ hasName "authors" /> hasName "name" >>> this &&& getAttrValue "role" >>> arr authorRole)))
            (none)))
    >>>
    arr last -- there may be several dates
    )

mkOpf metadata htmls images mathimages (coverimg, coverfilename) tocpagefile okuzukepagefile ncxfile cssfiles = runX (
  eelem "package"
  += sattr "xmlns" "http://www.idpf.org/2007/opf"
  += sattr "unique-identifier" "BookId"
  += sattr "version" "3.0"
  += (catA $ map constA metadata)
  += (eelem "manifest"
      += (eelem "item"
          += sattr "id" "coverId"
          += sattr "href" coverimg
          += sattr "media-type" "image/jpeg")
      += (eelem "item"
          += sattr "id" "titlepage"
          += sattr "href" coverfilename
          += sattr "media-type" "application/xhtml+xml")
      += (mkOpfCss cssfiles)
      += (eelem "item"
          += sattr "id" "ncx"
          += sattr "href" ncxfile
          += sattr "media-type" "application/x-dtbncx+xml")
      += (eelem "item"
          += sattr "properties" "nav"
          += sattr "id" "toc"
          += sattr "href" tocpagefile
          += sattr "media-type" "application/xhtml+xml")          
      += (eelem "item" 
          += sattr "id" "okuzuke"
          += sattr "href" okuzukepagefile
          += sattr "media-type" "application/xhtml+xml")
      += (mkOpfHtmls htmls)
      += (mkOpfImages images)
      += (mkOpfMathImages mathimages)
      += (eelem "item"
          += sattr "href" "fonts/Inconsolata.otf"
          += sattr "id" "font1"
          += sattr "media-type" "application/x-font-truetype")
      += (eelem "item"
          += sattr "href" "fonts/Inconsolatabold.otf"
          += sattr "id" "font2"
          += sattr "media-type" "application/x-font-truetype")
     )
  += (eelem "spine"
      += sattr "toc" "ncx"
      += mkOpfItemrefs htmls
      += (eelem "itemref"
          += sattr "idref" "okuzuke")
      += (eelem "itemref"
          += sattr "idref" "toc")
     )
  += (eelem "guide"
      += (eelem "reference"
          += sattr "href" coverfilename
          += sattr "title" "cover" 
          += sattr "type" "cover")
      += (eelem "reference"
          += sattr "href" tocpagefile
          += sattr "title" "table of contents" 
          += sattr "type" "toc")
     )
  )
  
mkOpfMetaData :: (ArrowXml a) => (String, (String, (String, String))) -> [(XmlTree, String)] -> a XmlTree XmlTree
mkOpfMetaData (title, (isbn, (rights, date))) authors =
  eelem "metadata" 
  += sattr "xmlns:dc" "http://purl.org/dc/elements/1.1/"
  += sattr "xmlns:opf" "http://www.idpf.org/2007/opf"
  += (eelem "dc:title"
      += txt title)
  += (eelem "dc:language"
      += txt "ja-JP")
  += (eelem "dc:identifier"
       += sattr "id" "BookId"
       += txt ("urn:isbn:"++isbn))
  += (eelem "meta"
      += sattr "property" "dcterms:modified"
      += txt date)
  += (eelem "meta"
      += (sattr "refines" "#BookId")
      += (sattr "property" "identifier-type")
      += sattr "scheme" "onix:codelint5" 
      += txt "15")
  += (eelem "dc:source"
       += sattr "id" "PbookId"
       += txt ("urn:isbn:"++isbn))
  += (eelem "meta"
      += (sattr "refines" "#PbookId")
      += (sattr "property" "identifier-type")
      += sattr "scheme" "onix:codelint5"
      += txt "15")
  += (eelem "dc:date"
      += (txt date))
  += (eelem "dc:rights"
      += txt rights)
  += (eelem "dc:publisher"
      += txt "ohmsha")
  += catA (map (\((name,role),id) -> (eelem "dc:creator" 
                                      += (sattr "id" ("creator"++(show id)))
                                      += (constA name >>> getChildren))
                                     <+>
                                     (eelem "meta" 
                                      += (sattr "refines" ("#creator"++(show id)))
                                      += (sattr "property" "role")
                                      += (sattr "scheme" "marc:relators")
                                      += (sattr "id" "role")
                                      += (txt role)))
           (zip authors [1..]))
  += (eelem "meta"
      += sattr "name" "cover"
      += sattr "content" "coverId")

mkOpfHtmls :: (ArrowXml a) => [(Int, String, String)] -> a XmlTree XmlTree
mkOpfHtmls htmls = catA $ 
                   map (\(n,t,f) -> eelem "item"
                                    += sattr "id" (t++(show n))
                                    += sattr "href" f
                                    += sattr "properties" "svg" -- FIXME
                                    += sattr "media-type" "application/xhtml+xml") $
                   htmls

mkOpfCss :: (ArrowXml a) => [String] -> a XmlTree XmlTree
mkOpfCss cssfiles = catA $
                    map (\(f, n) -> eelem "item"
                                    += sattr "id" ("css"++(show n))
                                    += sattr "href" f
                                    += sattr "media-type" "text/css") $
                    zip cssfiles [1..]


mkOpfImages :: (ArrowXml a) => [String] -> a XmlTree XmlTree
mkOpfImages images = catA $
                     map (\(s,n) -> eelem "item"
                                    += sattr "id" ("img"++(show n))
                                    += sattr "href" s
                                    += sattr "media-type" "image/png") $
                     (zip images [1..])

mkOpfMathImages :: (ArrowXml a) => [String] -> a XmlTree XmlTree
mkOpfMathImages images = catA $
                         map (\(s,n) -> eelem "item"
                              += sattr "id" ("mathimg"++(show n))
                              += sattr "href" s
                              += sattr "media-type" "image/svg+xml") $
                         (zip images [1..])
                                  
mkOpfItemrefs :: (ArrowXml a) => [(Int, String, String)] -> a XmlTree XmlTree
mkOpfItemrefs htmls = catA $ 
                      map (\(n,t,f) -> eelem "itemref"
                                       += sattr "idref" (t++(show n))) $
                      htmls

