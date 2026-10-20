{-# LANGUAGE Arrows, FlexibleContexts, BangPatterns #-}

module EPUB.MathReader (configEquations, mathElem, denseEqLabelsFromBodies) where

import Text.XML.HXT.Core hiding (xshow)

import qualified System.Process as Process
import System.Process (CreateProcess(..), StdStream(..))
import System.Exit (ExitCode(..))
import System.IO (hGetContents, hClose)
import System.IO.Error (ioError, userError)
import qualified System.FilePath.Posix as FP
import qualified System.Process as Prc (readProcess)
import Data.List (isPrefixOf, isInfixOf, nub)
import Data.Char (isSpace)
import qualified Data.Map as Map
import qualified Codec.Archive.Zip as ZIP
import System.Directory (doesFileExist)
import Control.Monad (void, unless)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent (getNumCapabilities)
import Data.Hashable (hash)

import qualified System.IO as IO

type AuxLabels = Map.Map String (String, String)

data ImgType = PNG | SVG

instance Show ImgType where
  show PNG = "png"
  show SVG = "svg"

instance Read ImgType where
  readsPrec _ x = case x of
    "png" -> [(PNG, "")]
    "svg" -> [(SVG, "")]

-- Equation labels use the eq: or eqn: prefix (see docs/html-input.md).
configEquations :: AuxLabels -> String -> [FilePath] -> IO ([FilePath], IO [ZIP.Entry], AuxLabels)
configEquations auxlabels mathtype filenames =
  case mathtype of
      x | elem x ["png", "svg"] -> do
          let imgtype = read x :: ImgType
          maths <- mapM (mathElemToResourceName imgtype) filenames
          let allMaths = concat maths
              -- PDF aux numbers skip unlabeled equations, so tagging only from aux
              -- yields gaps (2,5,11…). Renumber labeled eqs 1..N in document order
              -- and override \\newlabel so \\ref matches the visible tags.
              dense = denseEqLabelsFromBodies (map snd allMaths)
              labelmap = Map.union dense auxlabels
              prepared =
                [ (p, prepareEqBody dense b, equationNeedsRenumber b)
                | (p, b) <- allMaths
                ]
              -- One render job per content-hash path (avoid races / duplicate ZIP entries).
              uniquePrepared =
                Map.elems $ Map.fromList
                  [ (p, (p, equation, needs))
                  | (p, equation, needs) <- prepared
                  ]
          numCaps <- getNumCapabilities
          let maxConcurrency = max 1 (numCaps * 2)
          let batches = chunksOf maxConcurrency uniquePrepared
          mapM_ (\batch ->
            mapConcurrently_ (\(mathimagepath, equation, needs) -> do
              exists <- doesFileExist mathimagepath
              unless (exists && not needs) $
                void (genImageFromEqString labelmap imgtype mathimagepath equation)
              ) batch
            ) batches
          let mathimages = nub [p | (p, _, _) <- prepared]
          return $ (mathimages, mapM (ZIP.readEntry []) mathimages, dense)
      "mathml" -> return ([], return [], Map.empty)
  where
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)
    equationNeedsRenumber s =
      "\\ref" `isInfixOf` s || "\\eqref" `isInfixOf` s || "\\label" `isInfixOf` s
      || hasNumberedDisplay s
    hasNumberedDisplay s =
      any (\e -> ("\\begin{" ++ e ++ "}") `isInfixOf` s)
          ["equation", "align", "gather", "multline", "eqnarray"]

-- Labels in display math, document order → 1..N (EPUB-visible equation numbers).
denseEqLabelsFromBodies :: [String] -> AuxLabels
denseEqLabelsFromBodies bodies =
  Map.fromList
    [ (lab, (show n, ""))
    | (n, lab) <- zip [1 :: Int ..] (concatMap eqLabelsIn bodies)
    ]

eqLabelsIn :: String -> [String]
eqLabelsIn = go
  where
    go s =
      case findLabelSpan s of
        Nothing -> []
        Just (_, lab, after)
          | isEqLabel lab -> lab : go after
          | otherwise -> go after
    isEqLabel lab =
      "eq:" `isPrefixOf` lab || "eqn:" `isPrefixOf` lab

prepareEqBody :: AuxLabels -> String -> String
prepareEqBody dense = starNumberedEnvs . applyAuxTags dense

genImageFromEqString :: AuxLabels -> ImgType -> FilePath -> String -> IO ()
genImageFromEqString auxlabels mathtype imagepath equation = do
  let tempPrefix = "temp-" ++ show (abs (hash imagepath))
      tempTex = tempPrefix ++ ".tex"
      tempPdf = tempPrefix ++ ".pdf"
      tempCropPdf = tempPrefix ++ "-crop.pdf"
  IO.writeFile tempTex (latexTemplate auxlabels equation)
  (_, _, mhErr, ph1) <- Process.createProcess ((Process.proc "ptex2pdf"
        ["-u", "-l", "-ot", "-interaction=nonstopmode",
         "-output-directory=.", tempTex])
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
  case exitCode of
    ExitSuccess -> convertToImage mathtype imagepath tempPdf tempCropPdf
    _ -> do
      putStrLn $ "ptex2pdf exited with code: " ++ show exitCode ++ unlines (lines errOutput)
      ioError $ userError $ "ptex2pdf failed for " ++ imagepath
  (_, _, _, ph2) <- Process.createProcess ((Process.proc "sh" ["-c", "rm -f " ++ tempPrefix ++ "*"])
    { std_out = NoStream, std_err = NoStream })
  void $ Process.waitForProcess ph2
  where
    convertToImage PNG imagepath tempPdf tempCropPdf = do
      (_, _, _, ph1) <- Process.createProcess ((Process.proc "pdfcrop"
        ["--margins", "2", tempPdf, tempCropPdf])
        { std_out = NoStream, std_err = NoStream })
      void $ Process.waitForProcess ph1
      -- 200dpi is enough for EPUB screens; height/width→em/% scale with this.
      (_, _, _, ph2) <- Process.createProcess ((Process.proc "convert"
        ["-density", "200", "-quality", "100", "-background", "white",
         "-alpha", "remove", "-alpha", "off", "-type", "Grayscale",
         tempCropPdf ++ "[0]", imagepath])
        { std_out = NoStream, std_err = NoStream })
      void $ Process.waitForProcess ph2
    convertToImage SVG imagepath tempPdf tempCropPdf = do
      (_, _, _, ph1) <- Process.createProcess ((Process.proc "pdfcrop" [tempPdf, tempCropPdf])
        { std_out = NoStream, std_err = NoStream })
      void $ Process.waitForProcess ph1
      (_, _, _, ph2) <- Process.createProcess ((Process.proc "pdf2svg" [tempCropPdf, imagepath])
        { std_out = NoStream, std_err = NoStream })
      void $ Process.waitForProcess ph2
    latexTemplate labels body =
         "\\documentclass[dvipdfmx]{jsarticle}\n"
      ++ "\\usepackage{amsmath,amstext,amssymb,array}\n"
      ++ "\\pagestyle{empty}\n"
      ++ auxNewlabels labels
      ++ "\\begin{document}\n"
      ++ body
      ++ "\n\\end{document}\n"

-- Emit classic \\newlabel{lab}{{text}{page}} so isolated jobs resolve \\ref.
auxNewlabels :: AuxLabels -> String
auxNewlabels =
  concatMap (\(lab, (text, page)) ->
               "\\newlabel{" ++ lab ++ "}{{" ++ text ++ "}{" ++ page ++ "}}\n")
  . Map.toList

-- \\label{eq:foo} → \\tag{N}\\label{eq:foo} using the (dense) label map.
applyAuxTags :: AuxLabels -> String -> String
applyAuxTags labels s =
  case findLabelSpan s of
    Nothing -> s
    Just (before, lab, after) ->
      let tag = case Map.lookup lab labels of
                  Just (text, _)
                    | not (null text) && not (nearbyTag before) ->
                        "\\tag{" ++ text ++ "}"
                  _ -> ""
      in before ++ tag ++ "\\label{" ++ lab ++ "}" ++ applyAuxTags labels after

findLabelSpan :: String -> Maybe (String, String, String)
findLabelSpan [] = Nothing
findLabelSpan xs@(_:ys)
  | "\\label{" `isPrefixOf` xs =
      let rest = drop (length "\\label{") xs
          (lab, braceRest) = break (== '}') rest
      in if null braceRest
         then Nothing
         else Just ("", lab, drop 1 braceRest)
  | otherwise =
      case findLabelSpan ys of
        Nothing -> Nothing
        Just (b, lab, a) -> Just (head xs : b, lab, a)

nearbyTag :: String -> Bool
nearbyTag before =
  let tail40 = reverse (take 40 (reverse (dropWhileEnd isSpace before)))
  in "\\tag" `isInfixOf` tail40
  where
    dropWhileEnd p = reverse . dropWhile p . reverse

-- Isolated jobs must not auto-number from 1. Keep only \\tag{N} from aux labels.
starNumberedEnvs :: String -> String
starNumberedEnvs s = foldr starEnv s ["equation", "align", "gather", "multline", "eqnarray"]
  where
    starEnv name body = go body
      where
        begin = "\\begin{" ++ name ++ "}"
        end = "\\end{" ++ name ++ "}"
        beginS = "\\begin{" ++ name ++ "*}"
        endS = "\\end{" ++ name ++ "*}"
        go [] = []
        go xs
          | beginS `isPrefixOf` xs = beginS ++ go (drop (length beginS) xs)
          | endS `isPrefixOf` xs = endS ++ go (drop (length endS) xs)
          | begin `isPrefixOf` xs = beginS ++ go (drop (length begin) xs)
          | end `isPrefixOf` xs = endS ++ go (drop (length end) xs)
          | otherwise = head xs : go (tail xs)

-- Pandoc LaTeX→HTML may already wrap display math in \begin{equation} etc.
wrapDisplayMath :: String -> String
wrapDisplayMath body
  | any (`isPrefixOf` dropWhile isSpace body) latexEnvStarts = body
  | otherwise = "\\begin{align*}" ++ body ++ "\\end{align*}"
  where
    latexEnvStarts = map ("\\begin{" ++) ["equation", "align", "align*", "gather", "multline", "eqnarray"]

mathImgFileName :: ImgType -> String -> FilePath
mathImgFileName imgtype body =
  "eq" ++ show (abs (hash body)) FP.<.> show imgtype

mathImgPath :: ImgType -> String -> FilePath
mathImgPath imgtype body =
  "images" FP.</> show imgtype FP.</> mathImgFileName imgtype body

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
        (multi
          (choiceA [ hasName "eq" :->
                     (this /> getText >>> arr (\t ->
                       let body = "$" ++ t ++ "$"
                       in (mathImgPath imgtype body, body)))
                   , (hasName "math" <+> hasName "equation") :->
                     (this /> getText >>> arr (\t ->
                       let body = wrapDisplayMath t
                       in (mathImgPath imgtype body, body)))
                   , this :-> none
                   ])))

mathElem :: (ArrowXml a, ArrowIO a) => String -> FilePath -> a XmlTree XmlTree
mathElem mathtype filename =
  case mathtype of
    x | elem x ["png", "svg"] ->
      let imgtype = read x :: ImgType
      in do
           processBottomUp (
                ((mkMathSpan $< (this /> getText >>> arr (\t ->
                    mkMathImgSrcPath imgtype filename ("$" ++ t ++ "$"))))
                 `when` hasName "eq")
                >>>
                ((mkMathDiv $< (this /> getText >>> arr (\t ->
                    mkMathImgSrcPath imgtype filename (wrapDisplayMath t))))
                 `when` (hasName "equation")))
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

    mkMathImgSrcPath :: ImgType -> FilePath -> String -> FilePath
    mkMathImgSrcPath imgtype filename body
      = (mkMathImgLead filename) FP.</> mathImgPath imgtype body
    mkMathSpan :: (ArrowXml a) => FilePath -> a XmlTree XmlTree
    mkMathSpan filepath =
      eelem "span"
      += (eelem "img"
          += sattr "src" filepath
          += sattr "class" "inlinemath")

    mkMathDiv filepath =
      eelem "span"
      += (ifA (hasAttr "id") (sattr "id" $< getAttrValue "id") (none))
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
        -- Calibrated for PNG density 200.
        appropriateHeightOf imgtype imgfilepath = case imgtype of
          PNG -> arrIO0 $ Prc.readProcess "identify" ["-format", "\"%[fx:h/16*0.6]\"", imgfilepath] []
          SVG -> arrIO0 $ Prc.readProcess "identify" ["-format", "\"%[fx:h/16*0.6]\"", imgfilepath] []

        appropriateWidthOf imgtype imgfilepath = case imgtype of
          PNG -> arrIO0 $ Prc.readProcess "identify" ["-format", "\"%[fx:w/1000*100]\"", imgfilepath] []
          SVG -> arrIO0 $ Prc.readProcess "identify" ["-format", "\"%[fx:w/1000*100]\"", imgfilepath] []

        dropParents path = FP.joinPath $ filter (not . (isPrefixOf ".")) $ FP.splitPath path
