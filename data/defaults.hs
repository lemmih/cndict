{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception
import System.Process
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Char
import Data.Chinese.Pinyin
import Data.Maybe

-- 1. Load defaults.txt
-- 2. Find ambiguous words.
-- 3. For each word:
--  4. Find default with google translate.
--  5. Write defaults.txt

main :: IO ()
main = do
    defs <- readDefaults
    dict <- readCCDict "cedict_1_0_ts_utf-8_mdbg.txt"
    let ws = filter (`Map.notMember` defs) (ambiguous dict)
    print (length ws)
    print (length dict)
    worker defs ws
  where
    worker defs [] = return ()
    worker defs (w:ws) = do
      pinyin <- googlePinyin w
      putStrLn $ T.unpack w ++ " : " ++ T.unpack pinyin
      let defs' = Map.insert w pinyin defs
      writeDefaults defs'
      worker defs' ws


readDefaults :: IO (Map Text Text)
readDefaults = do
  inp <- handle (\(e::SomeException) -> return "") (T.readFile "defaults.txt")
  return $ Map.fromList
    [ (key, val)
    | line <- T.lines inp
    , [key, val] <- [T.splitOn ":" line]
    ]

writeDefaults :: Map Text Text -> IO ()
writeDefaults defs =
  writeFile "defaults.txt"
    $ unlines
    [ T.unpack key ++ ":" ++ T.unpack val
    | (key, val) <- Map.toList defs ]

ambiguous = worker Map.empty
  where
    worker _seen [] = []
    worker seen ((_trad, simpl, pinyin, defs):xs) =
      let thisPinyin = T.filter (not.isSpace) $ T.toLower pinyin in
      case Map.lookup simpl seen of
        Just "" ->
          worker seen xs
        Just prevPinyin | prevPinyin /= thisPinyin ->
          simpl : worker (Map.insert simpl "" seen) xs
        Just{} ->
          worker seen xs
        Nothing ->
          worker (Map.insert simpl thisPinyin seen) xs

googlePinyin :: Text -> IO Text
googlePinyin word =
  (T.toLower . T.strip . T.pack)
    <$> readProcess "./trans" ["-s","zh","-b",":@zh", T.unpack word] ""


-- Traditional, Simplified, Pinyin, Definitions
readCCDict :: FilePath -> IO [(Text,Text,Text,Text)]
readCCDict path = do
  txt <- T.readFile path
  return $ mapMaybe readCCEntry (T.lines txt)

readCCEntry :: Text -> Maybe (Text,Text,Text,Text)
readCCEntry line
  | "#" `T.isPrefixOf` line = Nothing
  | T.any (=='\t') english = error "tab in english"
  | otherwise =
    Just (traditional, simplified, pinyin', english)
  where
    (traditional, line') = T.breakOn " " line
    (simplified, line'') = T.breakOn " " (T.drop 1 line')
    (pinyin_, english_) = T.breakOn "/" (T.drop 1 line'')
    !english = T.dropAround isSpace english_
    !pinyin = T.dropAround (\c -> isSpace c || c == '[' || c == ']') pinyin_
    pinyin' = T.unwords $ map toToneMarks $ T.words pinyin
