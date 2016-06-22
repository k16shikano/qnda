{-# LANGUAGE Arrows, FlexibleContexts #-}

module EPUB.ImageReader where
 
import Text.XML.HXT.Core hiding (xshow)

import System.Directory (copyFile, createDirectoryIfMissing)
import qualified System.FilePath.Posix as FP
import qualified System.Process as Prc (readProcess)

import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified Data.Tree.Class as Tree
import qualified Debug.Trace as DT (trace)

imagePathToResourceName ::   FilePath      -- input file name
                          -> IO [FilePath] -- list of image paths within the file
imagePathToResourceName f = do
  links <- 
    runX (readDocument [ withIndent no
                       , withRemoveWS yes
                       , withValidate no
                       ] f
          >>>
          (listA $
           multi $ 
           hasName "img" >>> 
           (ifA (    hasAttrValue "class" (=="inlinemath") 
                 <+> hasAttrValue "class" (=="displaymath"))
            (none)
            (getAttrValue "src"))))
  paths <- mapM (
    \(basename, counter) -> do
      let htmlpath = FP.takeDirectory f
      if htmlpath /= "." then createDirectoryIfMissing True ("images/png/"++htmlpath) else return ()
      let imgpathInEpub = 
            "images/png/" ++ (FP.dropExtension f) ++ basename ++ (show counter) ++ ".png"
      copyFile ("images/png/"++basename++".png") imgpathInEpub
      return imgpathInEpub
    ) $ zip (map (FP.takeBaseName) $ concat links) [1..]
  return paths

mkImgId :: FilePath  -> (FilePath, Int) -> String
mkImgId f (s,c) = 
  FP.dropExtension f ++ FP.takeBaseName s ++ show c

imgAltAttr f (s,c) = sattr "alt" (mkImgId f (s,c+1))

mkImgLead :: FilePath -> String
mkImgLead f = 
  let htmlpath = FP.takeDirectory f
  in (concat . replicate (length $ (filter (/=".") $ FP.splitPath htmlpath))) "../"

mkImgSrcPath :: String -> String -> FilePath 
mkImgSrcPath imglead imgid = FP.combine (imglead++"images/png/") $ imgid FP.<.> ".png"

imgSrcAttr f l (s,c) = sattr "src" (mkImgSrcPath l (mkImgId f (s,c)))

imgElem :: (ArrowXml a) => FilePath -> a XmlTree XmlTree
imgElem f = 
   fromSLA 0 (
     processTopDown (
        (eelem "img" 
         += sattr "class" "figure"
         += (styleAttr $< getAttrValue "width" &&& getAttrValue "height")
         += (imgAltAttr f $< getAttrValue "src" &&& getState)
         += (imgSrcAttr f (mkImgLead f) $< getAttrValue "src" &&& nextState (+1)))
        `when`
        (hasName "img" >>> 
         neg (hasAttrValue "class" (=="inlinemath") <+> hasAttrValue "class" (=="displaymath")))))

styleAttr :: (ArrowXml a) => (String, String) -> a XmlTree XmlTree
styleAttr (w,h) = (case (w,h) of
                  ("","") -> sattr "style" "width:90%;"
                  ("",w')  -> sattr "style" $ "width:"++w'++";"
                  (h',"")  -> sattr "style" $ "height:"++h'++";"
                  (h',w')   -> sattr "style" $ "width:"++w'++";height:"++h'++";")


-- for test
tn = XN.mkElement (mkName "img") [ XN.mkAttr (mkName "width") [XN.mkText "10"]
                                 , XN.mkAttr (mkName "height") [XN.mkText "20"]
                                 , XN.mkAttr (mkName "src") [XN.mkText "path/to/image.png"]
                                 ] []

-- runLA (imgElem "foo/bar.html") tn

