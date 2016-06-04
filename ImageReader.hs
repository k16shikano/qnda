{-# LANGUAGE Arrows, FlexibleContexts #-}

module ImageReader where
 
import Text.XML.HXT.Core hiding (xshow)

import System.Directory (copyFile, createDirectoryIfMissing)
import qualified System.FilePath.Posix as FP
import qualified System.Process as Prc (readProcess)

-- import qualified Debug.Trace as DT (trace)

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

mkImgLead :: FilePath -> String
mkImgLead f = 
  let htmlpath = FP.takeDirectory f
  in (concat . replicate (length $ (filter (/=".") $ FP.splitPath htmlpath))) "../"

mkImgSrcPath :: String -> String -> FilePath 
mkImgSrcPath imglead imgid = FP.combine (imglead++"images/png/") $ imgid FP.<.> ".png"

imgElem :: (ArrowXml a) => FilePath -> a XmlTree XmlTree
imgElem f = 
   fromSLA 0 (
     processBottomUp (
        (((\(alt,path) -> 
            eelem "img" 
            += sattr "src" path
            += sattr "alt" alt
            += sattr "class" "figure")
          $<
          (getAttrValue "src" &&& nextState (+1) 
           >>> arr (mkImgId f)
           >>> (this &&& arr (mkImgSrcPath (mkImgLead f)))))
         >>> styleAttr)
        `when`
        (hasName "img" >>> neg (hasAttrValue "class" (=="inlinemath") <+> hasAttrValue "class" (=="displaymath")))))

styleAttr :: (ArrowXml a) => a XmlTree XmlTree
styleAttr = 
  ((\(h,w) -> (case (h,w) of
                  ("","") -> addAttr "style" "width:90%;"
                  ("",w)  -> addAttr "style" $ "width:"++w++";"
                  (h,"")  -> addAttr "style" $ "height:"++h++";"
                  (h,w)   -> addAttr "style" $ "width:"++w++";height:"++h++";"))
   $<$ ((getAttrValue "height") &&& (getAttrValue "width")))
  >>> removeAttr "height" >>> removeAttr "width" >>> removeAttr "abovespace"

