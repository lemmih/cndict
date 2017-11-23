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
import Control.Monad

-- These words cause problems for the word segmentation code.
-- They are odd words that are better left out of the dictionary.
blacklist :: [Text]
blacklist =
  [ "得很", "那是", "到了", "A"
  , "里人", "多事", "你我", "家的"
  , "Q", "这不", "你等", "我等" ]


data Type = Traditional | Simplified

main :: IO ()
main = do
  defs <- readDefaults
  txt <- T.readFile "dict.txt.big"
  let m = M.fromListWith max
            [ (word, count)
            | line <- T.lines txt
            , let [word, countStr, _ty] = T.words line
                  Right (count,_) = T.decimal countStr ]
  dict <- readCCDict "cedict_1_0_ts_utf-8_mdbg.txt"
  let smallest [] = 0
      smallest (x:xs) = foldr min x xs
      entries = concat
        [ if traditional == simplified
          then [(entry, Simplified, count)]
          else [(entry, Traditional, count)
               ,(entry, Simplified, count)]
        | entry@(traditional, simplified, pinyin, english) <- dict
        , not (traditional `elem` blacklist)
        , not (simplified `elem` blacklist)
        , let -- fallback 0 = smallest (catMaybes [ M.lookup c m | c <- T.chunksOf 1 simplified ])
              fallback other = other
        , let count = T.pack $ show $ fallback $
                M.findWithDefault 0 simplified m `max`
                M.findWithDefault 0 traditional m ]
      key ((traditional, _, _, _), Traditional, _) = traditional
      key ((_,simplified, _, _), Simplified, _) = simplified
      sorted = sortBy (comparing key) entries
      chunks = chunksOf 10000 sorted
      render ((traditional, simplified, pinyin, english), ty, count) =
        let isDef = if variantIsDefault defs simplified pinyin then "t" else "f" in
        case ty of
          _ | traditional == simplified ->
            T.intercalate "\t" [simplified, count, pinyin, isDef, english]
          Traditional ->
            T.intercalate "\t" [traditional, simplified, "T", count, pinyin, isDef, english]
          Simplified ->
            T.intercalate "\t" [simplified, traditional, "S", count, pinyin, isDef, english]
  forM_ (zip [0..] chunks) $ \(n,chunk) -> do
    let padded | n < 10    = '0':show n
               | otherwise = show n
        name = "dict-sorted-" ++ padded ++ ".txt"
    T.writeFile name $ T.unlines (map render chunk)
  --T.writeFile "dict.txt.big.sorted" $ T.unlines
  --  [ T.unwords [key, T.pack $ show val]
  --  | (key, val) <- M.toAscList m ]

variantIsDefault defs simplified pinyin =
  case M.lookup simplified defs of
    Nothing -> False
    Just defPinyin -> trim defPinyin == trim pinyin
  where
    trim = T.replace "'" "" . T.filter (not.isSpace) . T.toLower

readDefaults :: IO (M.Map Text Text)
readDefaults = do
  inp <- T.readFile "defaults.txt"
  return $ M.fromList
    [ (key, val)
    | line <- T.lines inp
    , [key, val] <- [T.splitOn ":" line]
    ]

chunksOf :: Int -> [e] -> [[e]]
chunksOf _ [] = []
chunksOf i ls =
  let (this,next) = splitAt i ls
  in this : chunksOf i next

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
