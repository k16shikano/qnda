{-# LANGUAGE Arrows, FlexibleContexts #-}

module MathReader where
 
import Text.XML.HXT.Core hiding (xshow)
import Text.XML.HXT.Arrow.XmlArrow hiding (xshow)
import Control.Arrow
import Control.Arrow.ArrowTree
import Control.Arrow.ArrowNavigatableTree
import Control.Arrow.ArrowIf
import qualified Text.XML.HXT.DOM.XmlNode as XN
import Text.XML.HXT.DOM.ShowXml (xshow)

import Data.Tree.Class (Tree)
import qualified Data.Tree.Class as T hiding (Tree)
import Data.Tree.NTree.TypeDefs (NTree)
import Data.Tree.NavigatableTree.Class

import Data.List
import Data.List.Split
import Data.Hashable ( hash )

import qualified Data.Map as Map

import Control.Monad as M hiding (when)
import Control.Monad.State as S hiding (when)

import Data.Maybe
import System.Environment
import System.Directory (copyFile)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Clock (getCurrentTime)
import System.Locale (defaultTimeLocale)
import Data.Time.Format (formatTime)

import qualified System.Cmd as Cmd (system)
import qualified System.FilePath.Posix as FP
import qualified System.Process as Prc (readProcess)

import qualified Data.ByteString.Lazy as B
import qualified System.IO as IO

import Data.ByteString.Lazy.UTF8 ( fromString, toString )
import Codec.Archive.Zip

import AuxReader (readAux)

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
                     ((nextState (+1) >>> arr (mkImagePath f))
                      &&& (this /> getText >>> arr (("\\HUGE$"++) . (++"$"))))
                   , (hasName "math" <+> hasName "equation") :->
                     ((nextState (+1) >>> arr (mkImagePath f))
                      &&& (this /> getText >>> arr (("\\begin{eqnarray*}"++) . (++"\\end{eqnarray*}"))))
                   , this :-> none
                   ]))))

mkImagePath filename seq = FP.combine "images" $ (FP.dropExtension filename ++ show seq FP.<.> "svg")

latexTemplate body = "\\documentclass[landscape]{jsbook}\n"
                    ++"\\usepackage{lucidabr,amsmath,amstext,amssymb,array,mymacro}\n"
                    ++"\\usepackage[deluxe, expert]{otf}\n\\pagestyle{empty}\n"
                    ++"\\begin{document}\n"
                    ++body
                    ++"\n\\end{document}\n"

genImageFromEqString imagepath equation = do
  IO.writeFile "temp" (latexTemplate equation)
  Cmd.system $ "scripts/mkMathImg.sh " ++ imagepath

mathElem :: (ArrowXml a, ArrowIO a) => FilePath -> a XmlTree XmlTree
mathElem filename =
  fromSLA 0 (
    processBottomUp (
       ((mkMathSpan $< ((nextState (+1) >>> arr (mkImagePath filename)))) 
        `when` hasName "eq")
       >>>
       ((mkMathDiv $< ((nextState (+1) >>> arr (mkImagePath filename)))) 
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
  eelem "div"
  += (eelem "img"
      += sattr "src" filepath
      += sattr "class" "displaymath")

setStyleForMath :: (ArrowXml a, ArrowIO a) => a XmlTree XmlTree
setStyleForMath =
  processBottomUp (
    choiceA 
    [ hasAttrValue "class" (=="displaymath") :->
      ((addAttr "style") . ("width:"++) . (++"%;") . (filter (`elem` "0123456789.")) $< (appropriateWidthOf $< getAttrValue "src"))
    , hasAttrValue "class" (=="inlinemath") :->
      ((addAttr "style") . ("height:"++) . (++"em;") . (filter (`elem` "0123456789.")) $< (appropriateHeightOf $< getAttrValue "src"))
    , this :-> this]
    `when` hasName "img")

-- for png
--appropriateHeightOf imgfilepath = arrIO0 $ Prc.readProcess "identify" ["-format", "\"%[fx:h/58]\"", imgfilepath] []
--appropriateWidthOf  imgfilepath = arrIO0 $ Prc.readProcess "identify" ["-format", "\"%[fx:w/800*100]\"", imgfilepath] []

appropriateHeightOf imgfilepath = arrIO0 $ Prc.readProcess "identify" ["-format", "\"%[fx:h/40]\"", imgfilepath] []
appropriateWidthOf  imgfilepath = arrIO0 $ Prc.readProcess "identify" ["-format", "\"%[fx:w/400*100]\"", imgfilepath] []