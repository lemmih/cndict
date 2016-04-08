{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Simplified Chinese <-> English dictionary with pinyin phonetics.
module Data.Chinese.CCDict
  ( initiate
  , Entry(..)
  , ppEntry
  , entryVariants
  , entrySimplified
  , entryTraditional
  , entryWordFrequency
  , entryPinyin
  , Variant(..)
  , lookupMatch
  , lookupMatches
  ) where

import           Data.Char
import           Data.Maybe
import           Data.Ord
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Paths_cndict
import           Prelude                hiding (lookup)
import           System.IO.Unsafe       (unsafePerformIO)


import qualified Data.Text.Internal as T
import qualified Data.Text.Array as T
import qualified Data.Array.Unboxed as U
import qualified Data.Text.Read        as T
import Control.Exception (evaluate)

-- | Load DB into memory. Otherwise it happens when the DB
--   is first used.
initiate :: IO ()
initiate = do
  evaluate ccDict
  return ()

data CCDict = CCDict !T.Array !Int (U.UArray Int Int)

mkCCDict :: Text -> CCDict
mkCCDict text@(T.Text arr _ len) =
    CCDict arr len
      (U.listArray (0,n-1) offsets)
  where
    ls = T.lines text
    offsets =
      [ offset
      | T.Text _ offset _length <- ls ]
    n = length ls

ccDict :: CCDict
ccDict = mkCCDict utfData
  where
    utfData = unsafePerformIO $ do
      path  <- getDataFileName "data/dict.sorted"
      T.readFile path

ccDictNth :: Int -> CCDict -> Text
ccDictNth n (CCDict arr totalLen offsets) =
    T.text arr offset len
  where
    lastIdx = snd (U.bounds offsets)
    offset = offsets U.! n
    len
      | lastIdx == n = totalLen - offset - 1
      | otherwise    = offsets U.! (n+1) - offset - 1

bounds :: CCDict -> (Int, Int)
bounds (CCDict _ _ offsets) = U.bounds offsets

findPrefix :: CCDict -> Int -> Int -> Int -> Text -> Maybe (Int,Int)
findPrefix dict maxUpper lower upper key
  | lower > upper = Nothing
  | otherwise =
    case compare (T.take len key) (T.take len val) of
      LT -> findPrefix dict (middle-1) lower (middle-1) key
      GT -> findPrefix dict maxUpper (middle+1) upper key
      EQ ->
        case compare (T.length key) (T.length val) of
          GT -> findPrefix dict maxUpper (middle+1) upper key
          _ -> Just $ fromMaybe (middle, maxUpper) $
                  findPrefix dict maxUpper lower (middle-1) key
  where
    middle = (upper - lower) `div` 2 + lower
    val = T.takeWhile (/='\t') $ ccDictNth middle dict
    len = min (T.length val) (T.length key)

lookupMatches :: Text -> Maybe [Entry]
lookupMatches key
  | T.null key = Nothing
lookupMatches key =
    if null entries
      then Nothing
      else Just entries
  where
    keys = tail $ T.inits key
    entries = worker (bounds ccDict) keys
    worker _ [] = []
    worker (lower, upper) (k:ks) =
      case findPrefix ccDict upper lower upper k of
        Nothing -> []
        Just (first, newUpper) ->
          maybe id (:) (scrapeEntry ccDict first k) $
          worker (first, newUpper) ks

lookupMatch :: Text -> Maybe Entry
lookupMatch key
  | T.null key = Nothing
  | otherwise =
    case findPrefix ccDict upper lower upper key of
      Nothing -> Nothing
      Just (first, _newUpper) ->
        scrapeEntry ccDict first key
    where
      (upper, lower) = bounds ccDict

scrapeEntry :: CCDict -> Int -> Text -> Maybe Entry
scrapeEntry dict nth key =
    case variants of
      [] -> Nothing
      (v:vs) -> Just (Entry v vs)
  where
    variants = scrapeVariants dict nth key

scrapeVariants :: CCDict -> Int -> Text -> [Variant]
scrapeVariants dict nth key
  | nth > snd (bounds dict) = []
  | T.takeWhile (/='\t') raw == key =
      parseVariant raw : scrapeVariants dict (nth+1) key
  | otherwise = []
    where
      raw = ccDictNth nth dict

parseVariant :: Text -> Variant
parseVariant line =
  case T.splitOn "\t" line of
    [chinese, count, pinyin, english] ->
      mkVariant chinese chinese count pinyin english
    [traditional, simplified, "T", count, pinyin, english] ->
      mkVariant traditional simplified count pinyin english
    [simplified, traditional, "S", count, pinyin, english] ->
      mkVariant traditional simplified count pinyin english
    _ -> error $ "invalid variant: " ++ T.unpack line
  where
    mkVariant traditional simplified countStr pinyin english = Variant
      { variantTraditional = traditional
      , variantSimplified = simplified
      , variantWordFrequency = count
      , variantPinyin = pinyin
      , variantDefinitions = splitDefinition english }
      where
        Right (count,_) = T.decimal countStr

--freqLookup_ :: FreqMap -> Text -> Maybe Int
--freqLookup_ freq key = worker (bounds freq)
--  where
--    worker (lower, upper)
--      | lower > upper = Nothing
--    worker (lower, upper) =
--      let middle = (upper - lower) `div` 2 + lower
--          val = freqNth middle freq
--          [word, countStr] = T.words val
--          Right (count,_) = T.decimal countStr
--      in case compare key word of
--        LT -> worker (lower, middle-1)
--        GT -> worker (middle+1, upper)
--        EQ -> Just count

--------------------------------------------------
-- Dictionary


-- | Dictionary entry
--data Entry = Entry
--  { entrySimplified  :: !Text
--  , entryTraditional :: !Text
--  , entryPinyin      :: [Text]
--  , entryDefinition  :: [[Text]]
--  } deriving ( Read, Show, Eq, Ord )

data Entry = Entry Variant [Variant]
  deriving (Show, Read, Eq, Ord)
data Variant = Variant
  { variantSimplified    :: !Text
  , variantTraditional   :: !Text
  , variantWordFrequency :: !Int
  , variantPinyin        :: !Text
  , variantDefinitions   :: [Text]
  } deriving ( Read, Show, Eq, Ord )

entryVariants :: Entry -> [Variant]
entryVariants (Entry v vs) = v:vs

dominantVariant :: Entry -> Variant
dominantVariant (Entry v vs) =
    foldr dom v vs
  where
    dom v1 v2
      | variantWordFrequency v1 < variantWordFrequency v2 =
        v2
      | otherwise =
        v1

entrySimplified :: Entry -> Text
entrySimplified = variantSimplified . dominantVariant

entryTraditional :: Entry -> Text
entryTraditional = variantTraditional . dominantVariant

entryWordFrequency :: Entry -> Int
entryWordFrequency = variantWordFrequency . dominantVariant

entryPinyin :: Entry -> [Text]
entryPinyin = map variantPinyin . entryVariants

ppEntry :: Entry -> Text
ppEntry = T.intercalate "\n" . map ppVariant . entryVariants

ppVariant :: Variant -> Text
ppVariant (Variant simplified traditional frequency pinyin english) =
  T.intercalate "\t"
    [simplified, traditional, count, pinyin, english']
  where
    count = T.pack $ show frequency
    english' = T.intercalate "/" english


-- /first/second/third/ -> [first, second, third]
splitDefinition :: Text -> [Text]
splitDefinition = filter (not . T.null) . T.splitOn "/" . T.dropAround isSpace


