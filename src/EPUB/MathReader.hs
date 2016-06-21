{-# LANGUAGE Arrows, FlexibleContexts #-}

module EPUB.MathReader where
 
import Text.XML.HXT.Core hiding (xshow)

import qualified System.Process as Process (system)
import qualified System.FilePath.Posix as FP
import qualified System.Process as Prc (readProcess)
import  Data.List (isPrefixOf)

import qualified System.IO as IO

import qualified Debug.Trace as DT (trace)

mathElemToResourceName ::    FilePath -- file name
                          -> IO [(FilePath -- resource path
                                 ,String   -- equation body (expected to be in LaTeX style)
                                 )]
mathElemToResourceName f = do
  runX (readDocument [ withIndent no
                     , withRemoveWS yes
                     , withValidate no
                     ] f
        >>>
        (fromSLA 0
         (multi
          (choiceA [ hasName "eq" :-> 
                     ((nextState (+1) >>> arr (mkMathImgPath f))
                      &&& (this /> getText >>> arr (("\\HUGE$"++) . (++"$"))))
                   , (hasName "math" <+> hasName "equation") :->
                     ((nextState (+1) >>> arr (mkMathImgPath f))
                      &&& (this /> getText >>> arr (("\\begin{align*}"++) . (++"\\end{align*}"))))
                   , this :-> none
                   ]))))

-- mkImagePath filename seq = FP.combine "images" $ (FP.dropExtension filename ++ show seq FP.<.> "png")

mkMathImgLead :: FilePath -> String
mkMathImgLead f = 
  let htmlpath = FP.takeDirectory f
  in (concat . replicate (length $ (filter (/=".") $ FP.splitPath htmlpath))) "../"

mkMathImgPath :: FilePath -> Int -> FilePath 
mkMathImgPath filename seq 
  = FP.combine "images/png/" $ (FP.dropExtension filename ++ show seq FP.<.> ".png")

mkMathImgSrcPath :: FilePath -> Int -> FilePath 
mkMathImgSrcPath filename seq 
  = FP.combine ((mkMathImgLead filename)++"images/png/") $ (FP.dropExtension filename ++ show seq FP.<.> ".png")

latexTemplate body = "\\documentclass[landscape,uplatex]{jsbook}\n"
                    ++"\\usepackage{type1cm}\\usepackage[T1]{fontenc}"
                    ++"\\usepackage{textcomp,hiraprop}"
                    ++"\\usepackage[expert,lucidasmallscale,mathitalic2]{lucidabr}"
                    ++"\\usepackage{amsmath,amstext,amssymb,array}\n"
                    ++"\\pagestyle{empty}\n"
                    ++"\\begin{document}\n"
                    ++ body
                    ++"\n\\end{document}\n"

genImageFromEqString imagepath equation = do
  IO.writeFile "temp" (latexTemplate equation)
--  Process.system $ "mkMathImg.sh " ++ imagepath
  Process.system $ 
       "uplatex temp > /dev/null ;"
    ++ "dvipdfmx -q -f lucida -l temp.dvi ;" -- > /dev/null;"
    ++ "convert -strip -trim -density 200 +repage temp.pdf "
    ++ imagepath
  Process.system "rm -fr temp*"

mathElem :: (ArrowXml a, ArrowIO a) => FilePath -> a XmlTree XmlTree
mathElem filename =
  fromSLA 0 (
    processBottomUp (
       ((mkMathSpan $< ((nextState (+1) >>> arr (mkMathImgSrcPath filename)))) 
        `when` hasName "eq")
       >>>
       ((mkMathDiv $< ((nextState (+1) >>> arr (mkMathImgSrcPath filename)))) 
        `when` (hasName "equation" <+> hasName "math"))))
  >>>
  setStyleForMath

mkMathSpan :: (ArrowXml a) => FilePath -> a XmlTree XmlTree
mkMathSpan filepath = 
  eelem "span"
  += (eelem "img"
      += sattr "src" filepath
      += sattr "class" "inlinemath")

mkMathDiv filepath = 
  eelem "span"
  += (eelem "img"
      += sattr "src" filepath
      += sattr "class" "displaymath")

setStyleForMath :: (ArrowXml a, ArrowIO a) => a XmlTree XmlTree
setStyleForMath =
  processBottomUp (
    choiceA 
    [ hasAttrValue "class" (=="displaymath") :->
      ((addAttr "style") . ("width:"++) . (++"%;") . (filter (`elem` "0123456789.")) $< ((appropriateWidthOf . dropParents) $< getAttrValue "src"))
    , hasAttrValue "class" (=="inlinemath") :->
      ((addAttr "style") . ("height:"++) . (++"em;") . (filter (`elem` "0123456789.")) $< ((appropriateHeightOf . dropParents) $< getAttrValue "src"))
    , this :-> this]
    `when` hasName "img")

-- for png
appropriateHeightOf imgfilepath = arrIO0 $ Prc.readProcess "identify" ["-format", "\"%[fx:h/58]\"", imgfilepath] []
appropriateWidthOf  imgfilepath = arrIO0 $ Prc.readProcess "identify" ["-format", "\"%[fx:w/800*100]\"", imgfilepath] []

-- appropriateHeightOf imgfilepath = arrIO0 $ Prc.readProcess "identify" ["-format", "\"%[fx:h/40]\"", imgfilepath] []
-- appropriateWidthOf  imgfilepath = arrIO0 $ Prc.readProcess "identify" ["-format", "\"%[fx:w/400*100]\"", imgfilepath] []


dropParents path = FP.joinPath $ filter (not . (isPrefixOf ".")) $ FP.splitPath path

