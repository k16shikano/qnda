{-# LANGUAGE CPP #-}

module AuxReader (readAux) where

import System.Environment
import qualified Data.Map as Map
#if __GLASGOW_HASKELL__<612
import qualified System.IO.UTF8 as U
#endif
import qualified System.IO as IO

import Text.ParserCombinators.Parsec hiding (many, (<|>))
import Control.Applicative

readAux :: String -> IO (Map.Map String -- label
                                 ( String -- text string
                                 , String -- page nomble
                                 ))
readAux file = do
  lines <- getEveryLines file
  entries <- mapM getEntryFromLine lines
  return $ Map.fromList entries

getEntryFromLine line = do
  case (parse parseAux "" line) of
    Left err -> return ("nil", ("", ""))
    Right m -> return m

getEveryLines file = do
  inh <- IO.openFile file IO.ReadMode
#if __GLASGOW_HASKELL__<612
  body <- U.hGetContents inh
#else
  body <- IO.hGetContents inh
#endif
  result <- case parse (sepBy1 (many $ noneOf "\n") (string "\n")) "" body of
    Left err -> error $ show err
    Right m -> return m
  IO.hClose inh
  return result

parseAux :: Parser (String, (String, String))
parseAux = gather <$> ((string "\\newlabel") *> braced <* string "{") <*> (braced <* braced) <*> braced
  where gather anchor counter title = (anchor, (counter, (trimTeX title)))

braced :: Parser String
braced = between (string "{") (string "}") (many $ noneOf "}")

trimTeX :: String -> String
trimTeX raw = case parse (concat <$> many1 noTeX) "" raw of
  Left err -> ""
  Right str -> str

noTeX :: Parser String
noTeX = concat <$> (sepEndBy1 (many1 $ noneOf "\\{}") (braced <|> (string "\\" >> many1 letter)))
