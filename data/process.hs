{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Read        as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.Char
import Data.Chinese.Pinyin
import Data.Maybe
import Data.List
import Data.Ord

-- These words cause problems for the word segmentation code.
-- They are odd words that are better left out of the dictionary.
blacklist :: [Text]
blacklist =
  [ "得很", "那是", "到了", "A"
  , "里人", "多事", "你我", "家的" ]


data Type = Traditional | Simplified

main :: IO ()
main = do
  txt <- T.readFile "dict.txt.big"
  let m = M.fromListWith max
            [ (word, count)
            | line <- T.lines txt
            , let [word, countStr, _ty] = T.words line
                  Right (count,_) = T.decimal countStr ]
  dict <- readCCDict "cedict_1_0_ts_utf-8_mdbg.txt"
  let entries = concat
        [ if traditional == simplified
          then [(entry, Simplified, count)]
          else [(entry, Traditional, count)
               ,(entry, Simplified, count)]
        | entry@(traditional, simplified, pinyin, english) <- dict
        , not (traditional `elem` blacklist)
        , not (simplified `elem` blacklist)
        , let count = T.pack $ show $
                M.findWithDefault 0 simplified m `max`
                M.findWithDefault 0 traditional m ]
      key ((traditional, _, _, _), Traditional, _) = traditional
      key ((_,simplified, _, _), Simplified, _) = simplified
      sorted = sortBy (comparing key) entries
  T.writeFile "dict.sorted" $ T.unlines
    [ case ty of
        _ | traditional == simplified ->
          T.intercalate "\t" [simplified, count, pinyin, english]
        Traditional ->
          T.intercalate "\t" [traditional, simplified, "T", count, pinyin, english]
        Simplified ->
          T.intercalate "\t" [simplified, traditional, "S", count, pinyin, english]
    | ((traditional, simplified, pinyin, english), ty, count) <- sorted ]
  --T.writeFile "dict.txt.big.sorted" $ T.unlines
  --  [ T.unwords [key, T.pack $ show val]
  --  | (key, val) <- M.toAscList m ]

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

{-
1. read freq db
2. read dict db
3. duplicate entries when traditional /= simplified
4. sort all entries
5. save them with frequency info attached


look algorithm
Key: ABCDE...

findPrefix :: Array -> (Int,Int) -> Text -> Maybe (Int, Int)

Find A or index of first word starting with A
If found A, add to result list
Find AB or index of first word starting with AB
If found AB, add to result list

-}
