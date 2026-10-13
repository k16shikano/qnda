{-# LANGUAGE Arrows, FlexibleContexts, BangPatterns #-}

module EPUB.MathReader (configEquations, mathElem) where

import Text.XML.HXT.Core hiding (xshow)

import qualified System.Process as Process
import System.Process (CreateProcess(..), StdStream(..))
import System.Exit (ExitCode(..))
import System.IO (hGetContents, hClose)
import qualified System.FilePath.Posix as FP
import qualified System.Process as Prc (readProcess)
import  Data.List (isPrefixOf)
import qualified Codec.Archive.Zip as ZIP
import System.Directory (doesFileExist)
import Control.Monad (void, unless)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent (getNumCapabilities)
import Data.Hashable (hash)

import qualified System.IO as IO

import qualified Debug.Trace as DT (trace)

data ImgType = PNG | SVG

instance Show ImgType where
  show PNG = "png"
  show SVG = "svg"

instance Read ImgType where
  readsPrec _ x = case x of
    "png" -> [(PNG, "")]
    "svg" -> [(SVG, "")]

configEquations :: String -> [FilePath] -> IO ([FilePath], IO [ZIP.Entry])
configEquations mathtype filenames =
  case mathtype of
      x | elem x ["png", "svg"] -> do
          let imgtype = read x :: ImgType
          maths <- mapM (mathElemToResourceName imgtype) filenames
          let allMaths = concat maths
          numCaps <- getNumCapabilities
          let maxConcurrency = max 1 (numCaps * 2)
          let batches = chunksOf maxConcurrency allMaths
          mapM_ (\batch ->
            mapConcurrently_ (\(mathimagepath, equation) -> do
              exists <- doesFileExist mathimagepath
              unless exists $
                void (genImageFromEqString imgtype mathimagepath equation)
              ) batch
            ) batches
          let mathimages = map fst allMaths
          return $ (mathimages, mapM (ZIP.readEntry []) mathimages)
      "mathml" -> return ([], return [])
  where
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- generate euations as images

genImageFromEqString :: ImgType -> FilePath -> String -> IO ()
genImageFromEqString mathtype imagepath equation = do
  let tempPrefix = "temp-" ++ show (abs (hash imagepath))
      tempTex = tempPrefix ++ ".tex"
      tempPdf = tempPrefix ++ ".pdf"
      tempCropPdf = tempPrefix ++ "-crop.pdf"
  IO.writeFile tempTex (latexTemplate equation)
  (_, _, mhErr, ph1) <- Process.createProcess ((Process.proc "pdflatex"
        ["-interaction=nonstopmode", "-output-directory=.", tempTex])
        { std_out = NoStream, std_err = CreatePipe })
  errOutput <- case mhErr of
    Just hErr -> do
      contents <- hGetContents hErr
      let !_ = length contents `seq` contents
      return contents
    Nothing -> return ""
  exitCode <- Process.waitForProcess ph1
  case mhErr of
    Just hErr -> hClose hErr
    Nothing -> return ()
  unless (exitCode == ExitSuccess) $
    putStrLn $ "pdflatex exited with code: " ++ show exitCode ++ unlines (lines errOutput)
  convertToImage mathtype imagepath tempPdf tempCropPdf
  (_, _, _, ph2) <- Process.createProcess ((Process.proc "sh" ["-c", "rm -f " ++ tempPrefix ++ "*"])
    { std_out = NoStream, std_err = NoStream })
  void $ Process.waitForProcess ph2
  where
    convertToImage PNG imagepath tempPdf _ = do
      (_, _, _, ph) <- Process.createProcess ((Process.proc "convert"
        ["-strip", "-trim", "-density", "200", "+repage", tempPdf, "-flatten", imagepath])
        { std_out = NoStream, std_err = NoStream })
      void $ Process.waitForProcess ph
    convertToImage SVG imagepath tempPdf tempCropPdf = do
      (_, _, _, ph1) <- Process.createProcess ((Process.proc "pdfcrop" [tempPdf, tempCropPdf])
        { std_out = NoStream, std_err = NoStream })
      void $ Process.waitForProcess ph1
      (_, _, _, ph2) <- Process.createProcess ((Process.proc "pdf2svg" [tempCropPdf, imagepath])
        { std_out = NoStream, std_err = NoStream })
      void $ Process.waitForProcess ph2
    latexTemplate body = "\\documentclass{standalone}\n"
                    ++"\\usepackage{type1cm}\\usepackage[T1]{fontenc}"
                    ++"\\usepackage{textcomp}"
                    ++"\\usepackage{amsmath,amstext,amssymb,array}\n"
                    ++"\\pagestyle{empty}\n"
                    ++"\\begin{document}\n"
                    ++ body
                    ++"\n\\end{document}\n"

-- equation settings in resource files

mathElemToResourceName ::    ImgType -- image type
                          -> FilePath -- file name
                          -> IO [(FilePath -- resource path
                                 ,String   -- equation body (expected to be in LaTeX style)
                                 )]
mathElemToResourceName imgtype f = do
  runX (readDocument [ withIndent no
                     , withRemoveWS yes
                     , withValidate no
                     ] f
        >>>
        (fromSLA 0
         (multi
          (choiceA [ hasName "eq" :->
                     ((nextState (+1) >>> arr (mkMathImgPath imgtype f))
                      &&& (this /> getText >>> arr (("\\HUGE$"++) . (++"$"))))
                   , (hasName "math" <+> hasName "equation") :->
                     ((nextState (+1) >>> arr (mkMathImgPath imgtype f))
                      &&& (this /> getText >>> arr (("\\begin{align*}"++) . (++"\\end{align*}"))))
                   , this :-> none
                   ]))))
  where
    mkMathImgPath :: ImgType -> FilePath -> Int -> FilePath
    mkMathImgPath imgtype filename seq
      = "images" FP.</> (show imgtype) FP.</> (FP.dropExtension filename ++ show seq FP.<.> (show imgtype))

-- set equations in HTML

mathElem :: (ArrowXml a, ArrowIO a) => String -> FilePath -> a XmlTree XmlTree
mathElem mathtype filename =
  case mathtype of
    x | elem x ["png", "svg"] ->
      let imgtype = read x :: ImgType
      in do
           fromSLA 0 (
             processBottomUp (
                ((mkMathSpan $< ((nextState (+1) >>> arr (mkMathImgSrcPath imgtype filename))))
                 `when` hasName "eq")
                >>>
                ((mkMathDiv $< ((nextState (+1) >>> arr (mkMathImgSrcPath imgtype filename))))
                 `when` (hasName "equation"))))
           >>>
           setStyleForMath imgtype
    "mathml" ->
      ((eelem "math" += (getChildren >>> (ifA (hasName "semantics") (getChildren >>> (ifA (hasName "annotation") (none) (this))) (none))))
       `when` hasName "math")

  where

    mkMathImgLead :: FilePath -> String
    mkMathImgLead f =
      let htmlpath = FP.takeDirectory f
      in (concat . replicate (length $ (filter (/=".") $ FP.splitPath htmlpath))) "../"

    mkMathImgSrcPath :: ImgType -> FilePath -> Int -> FilePath
    mkMathImgSrcPath imgtype filename seq
      = (mkMathImgLead filename) FP.</> "images" FP.</> (show imgtype) FP.</> (FP.dropExtension filename ++ show seq FP.<.> (show imgtype))

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

    setStyleForMath :: (ArrowXml a, ArrowIO a) => ImgType -> a XmlTree XmlTree
    setStyleForMath imgtype =
      processBottomUp (
        choiceA
        [ hasAttrValue "class" (=="displaymath") :->
          ((addAttr "style") . ("width:"++) . (++"%;") . (filter (`elem` "0123456789.")) $< ((appropriateWidthOf imgtype . dropParents) $< getAttrValue "src"))
        , hasAttrValue "class" (=="inlinemath") :->
          ((addAttr "style") . ("height:"++) . (++"em;") . (filter (`elem` "0123456789.")) $< ((appropriateHeightOf imgtype . dropParents) $< getAttrValue "src"))
        , this :-> this]
        `when` hasName "img")
      where
        appropriateHeightOf imgtype imgfilepath = case imgtype of
          PNG -> arrIO0 $ Prc.readProcess "identify" ["-format", "\"%[fx:h/58]\"", imgfilepath] []
          SVG -> arrIO0 $ Prc.readProcess "identify" ["-format", "\"%[fx:h/24]\"", imgfilepath] []

        appropriateWidthOf imgtype imgfilepath = case imgtype of
          PNG -> arrIO0 $ Prc.readProcess "identify" ["-format", "\"%[fx:w/800*100]\"", imgfilepath] []
          SVG -> arrIO0 $ Prc.readProcess "identify" ["-format", "\"%[fx:w/300*100]\"", imgfilepath] []

        dropParents path = FP.joinPath $ filter (not . (isPrefixOf ".")) $ FP.splitPath path
