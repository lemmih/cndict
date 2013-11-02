{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
module Data.Chinese.Pinyin
  ( toToneMarks
  , fromToneMarks
  ) where

import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Text  (Text)
import qualified Data.Text  as T

toToneMarks :: Text -> Text
toToneMarks = modToneNumber toTonal

fromToneMarks :: Text -> Text
fromToneMarks = error "Data.Chinese.Pinyin.fromToneMarks: undefined."

modToneNumber :: (Int -> Char -> Char) -> Text -> Text
modToneNumber fn txt
  | T.null txt || not (isDigit (T.last txt)) = txt
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
    case Prelude.lookup key lst of
      Nothing    -> key
      Just tones -> tones !! (n-1)
  where
    lst =
      [ ('a', "āáǎàa")
      , ('o', "ōóǒòo")
      , ('e', "ēéěèe")
      , ('i', "īíǐìi")
      , ('u', "ūúǔùu") ]

