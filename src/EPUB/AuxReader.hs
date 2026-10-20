{-# LANGUAGE CPP, BangPatterns #-}

module EPUB.AuxReader (readAux) where

import qualified Data.Map as Map
import qualified System.IO as IO
import System.FilePath (takeDirectory, (</>), isAbsolute)
import System.Directory (doesFileExist)

import Text.ParserCombinators.Parsec hiding (many, (<|>))
import Control.Applicative
import Control.Monad (foldM)

-- | Read \\newlabel entries from a .aux file, recursively following \\@input{…}.
readAux :: String -> IO (Map.Map String -- label
                                 ( String -- reference text (first \\newlabel field)
                                 , String -- page / second field
                                 ))
readAux file = do
  exists <- doesFileExist file
  if not exists
    then return Map.empty
    else do
      lines <- getEveryLines (takeDirectory file) file
      entries <- mapM getEntryFromLine lines
      return $ Map.fromList $ filter (\(k, _) -> k /= "nil") entries

getEntryFromLine line = case parse parseAux "" line of
  Left _ -> return ("nil", ("", ""))
  Right m -> return m

-- Collect logical lines from aux, expanding \\@input{file}.
getEveryLines :: FilePath -> FilePath -> IO [String]
getEveryLines baseDir file = go [] file
  where
    go seen f
      | f `elem` seen = return []
      | otherwise = do
          exists <- doesFileExist f
          if not exists
            then return []
            else do
              inh <- IO.openFile f IO.ReadMode
              body <- IO.hGetContents inh
              let rawLines = case parse (sepBy (many $ noneOf "\n") (string "\n")) "" body of
                    Left _ -> []
                    Right m -> m
              -- force read before close
              let !_ = length body
              IO.hClose inh
              foldM
                (\acc line -> do
                   case parseInput line of
                     Just rel -> do
                       let child = if isAbsolute rel then rel else baseDir </> rel
                       more <- go (f : seen) child
                       return (acc ++ more)
                     Nothing -> return (acc ++ [line]))
                []
                rawLines

parseInput :: String -> Maybe String
parseInput line = case parse p "" line of
  Right path -> Just path
  Left _ -> Nothing
  where
    p = spaces *> (string "\\@input" <|> string "\\input") *> braced <* spaces <* eof

parseAux :: Parser (String, (String, String))
parseAux = gather <$> (string "\\newlabel" *> braced <* string "{") <*> braced <*> braced
  where gather anchor counter page = (anchor, (counter, page))

braced :: Parser String
braced = between (string "{") (string "}") (many $ noneOf "}")
