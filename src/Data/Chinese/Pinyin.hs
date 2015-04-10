{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
module Data.Chinese.Pinyin
  ( restoreUmlaut
  , toToneMarks
  , fromToneMarks
  , clearToneMarks
  ) where

import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Text  (Text)
import qualified Data.Text  as T

restoreUmlaut :: Text -> Text
restoreUmlaut =
  T.replace "u:" "ü" .
  T.replace "ū:" "ǖ" .
  T.replace "ú:" "ǘ" .
  T.replace "ǔ:" "ǚ" .
  T.replace "ù:" "ǜ"

toToneMarks :: Text -> Text
toToneMarks = restoreUmlaut . modToneNumber toTonal

fromToneMarks :: Text -> Text
fromToneMarks txt = clearToneMarks $
  case wordToneNumber txt of
    Nothing -> txt
    Just n  -> txt `T.append` T.pack (show n)

modToneNumber :: (Int -> Char -> Char) -> Text -> Text
modToneNumber fn txt
  | T.null txt || not (isDigit (T.last txt)) ||
    T.last txt > '5' = txt
  | Just n <- T.findIndex (`elem` "ae") txt' = modify n
  | Just n <- findStrIndex "ou" txt'         = modify n
  | Just n <- findSecondVowel txt'           = modify n
  | Just n <- T.findIndex (`elem` "aoeiu") txt' = modify n
  | otherwise = T.init txt
  where
    tone = digitToInt (T.last txt)
    modify n = T.pack [ if n==i then fn tone c else c | (i,c) <- zip [0..] (T.unpack txt') ]
    txt' = T.init txt

findSecondVowel :: Text -> Maybe Int
findSecondVowel = listToMaybe . drop 1 . findIndices isVowel . T.unpack
  where
    isVowel = (`elem` "aoeiu")

findStrIndex :: Text -> Text -> Maybe Int
findStrIndex key = worker 0
  where
    worker n line
      | T.null line             = Nothing
      | key `T.isPrefixOf` line = Just n
      | otherwise               = worker (n+1) (T.drop 1 line)

toTonal :: Int -> Char -> Char
toTonal n key =
    case Prelude.lookup key toneList of
      Nothing    -> key
      Just tones
        | n < length tones -> tones !! (n-1)
        | otherwise        -> key
  where

toneList :: [(Char, String)]
toneList =
      [ ('a', "āáǎàa")
      , ('o', "ōóǒòo")
      , ('e', "ēéěèe")
      , ('i', "īíǐìi")
      , ('u', "ūúǔùu") ]

wordToneNumber :: Text -> Maybe Int
wordToneNumber txt = listToMaybe
  [ n
  | (n, str) <- zip [1..] (transpose (map (take 4 . snd) toneList))
  , elt <- str
  , T.count (T.singleton elt) txt > 0 ]

clearToneMarks :: Text -> Text
clearToneMarks = T.map worker
  where
    worker c = fromMaybe c (lookup c assocs)
    assocs =
      [ (elt, clear)
      | (clear, marked) <- toneList
      , elt <- marked ]
