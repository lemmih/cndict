{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Simplified Chinese <-> English dictionary with pinyin phonetics.
module Data.Chinese.CCDict
  ( initiate
  , version
  , Entry(..)
  , ppEntry
  , entryVariants
  , entryOriginal
  , entrySimplified
  , entryTraditional
  , entryWordFrequency
  , entryPinyin
  , entryDefaultPinyin
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
import qualified Data.Text.Encoding           as T
import           Paths_cndict
import           Prelude                hiding (lookup)
import           System.IO.Unsafe       (unsafePerformIO)
import Data.FileEmbed

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

-- GHC sucks at dealing with large strings so we have to split the dictionary
-- into smaller chunks.
ccDict :: CCDict
ccDict = mkCCDict (T.concat
    [utfData00, utfData01, utfData02, utfData03, utfData04, utfData05, utfData06
    ,utfData07, utfData08, utfData09, utfData10, utfData11, utfData12, utfData13
    ,utfData14, utfData15, utfData16, utfData17, utfData18])
  where
    utfData00 = T.decodeUtf8 $(embedFile "data/dict-sorted-00.txt")
    utfData01 = T.decodeUtf8 $(embedFile "data/dict-sorted-01.txt")
    utfData02 = T.decodeUtf8 $(embedFile "data/dict-sorted-02.txt")
    utfData03 = T.decodeUtf8 $(embedFile "data/dict-sorted-03.txt")
    utfData04 = T.decodeUtf8 $(embedFile "data/dict-sorted-04.txt")
    utfData05 = T.decodeUtf8 $(embedFile "data/dict-sorted-05.txt")
    utfData06 = T.decodeUtf8 $(embedFile "data/dict-sorted-06.txt")
    utfData07 = T.decodeUtf8 $(embedFile "data/dict-sorted-07.txt")
    utfData08 = T.decodeUtf8 $(embedFile "data/dict-sorted-08.txt")
    utfData09 = T.decodeUtf8 $(embedFile "data/dict-sorted-09.txt")
    utfData10 = T.decodeUtf8 $(embedFile "data/dict-sorted-10.txt")
    utfData11 = T.decodeUtf8 $(embedFile "data/dict-sorted-11.txt")
    utfData12 = T.decodeUtf8 $(embedFile "data/dict-sorted-12.txt")
    utfData13 = T.decodeUtf8 $(embedFile "data/dict-sorted-13.txt")
    utfData14 = T.decodeUtf8 $(embedFile "data/dict-sorted-14.txt")
    utfData15 = T.decodeUtf8 $(embedFile "data/dict-sorted-15.txt")
    utfData16 = T.decodeUtf8 $(embedFile "data/dict-sorted-16.txt")
    utfData17 = T.decodeUtf8 $(embedFile "data/dict-sorted-17.txt")
    utfData18 = T.decodeUtf8 $(embedFile "data/dict-sorted-18.txt")
    -- unsafePerformIO $ do
    --   path  <- getDataFileName "data/dict.sorted"
    --   T.readFile path

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
      (lower, upper) = bounds ccDict

allVariants :: [Variant]
allVariants = worker 0
  where
    worker nth | nth > snd (bounds ccDict) = []
    worker nth = parseVariant (ccDictNth nth ccDict) : worker (nth+1)

scrapeEntry :: CCDict -> Int -> Text -> Maybe Entry
scrapeEntry dict nth key =
    case variants of
      [] -> Nothing
      (v:vs) -> Just (Entry key v vs)
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
    [chinese, count, pinyin, isDef, english] ->
      mkVariant chinese chinese count pinyin isDef english
    [traditional, simplified, "T", count, pinyin, isDef, english] ->
      mkVariant traditional simplified count pinyin isDef english
    [simplified, traditional, "S", count, pinyin, isDef, english] ->
      mkVariant traditional simplified count pinyin isDef english
    _ -> error $ "invalid variant: " ++ T.unpack line
  where
    mkVariant traditional simplified countStr pinyin isDef english = Variant
      { variantTraditional = traditional
      , variantSimplified = simplified
      , variantWordFrequency = count
      , variantPinyin = pinyin
      , variantIsDefault = isDef == "t"
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

data Entry = Entry !Text Variant [Variant]
  deriving (Show, Read, Eq, Ord)
data Variant = Variant
  { variantSimplified    :: !Text
  , variantTraditional   :: !Text
  , variantWordFrequency :: !Int
  , variantPinyin        :: !Text
  , variantIsDefault     :: !Bool
  , variantDefinitions   :: [Text]
  } deriving ( Read, Show, Eq, Ord )

entryOriginal :: Entry -> Text
entryOriginal (Entry o _v _vs) = o

entryVariants :: Entry -> [Variant]
entryVariants (Entry _o v vs) = v:vs

dominantVariant :: Entry -> Variant
dominantVariant (Entry _o v vs) =
    foldr dom v vs
  where
    dom v1 v2
      | variantWordFrequency v1 < variantWordFrequency v2 =
        v2
      | otherwise =
        v1

defaultVariant :: Entry -> Maybe Variant
defaultVariant = listToMaybe . filter variantIsDefault . entryVariants

entrySimplified :: Entry -> Text
entrySimplified = variantSimplified . dominantVariant

entryTraditional :: Entry -> Text
entryTraditional = variantTraditional . dominantVariant

entryWordFrequency :: Entry -> Int
entryWordFrequency = variantWordFrequency . dominantVariant

entryPinyin :: Entry -> [Text]
entryPinyin = map variantPinyin . entryVariants

entryDefaultPinyin :: Entry -> Text
entryDefaultPinyin e = variantPinyin $
  fromMaybe (dominantVariant e) (defaultVariant e)

ppEntry :: Entry -> Text
ppEntry = T.intercalate "\n" . map ppVariant . entryVariants

ppVariant :: Variant -> Text
ppVariant (Variant simplified traditional frequency pinyin isDef english) =
  T.intercalate "\t"
    [simplified, traditional, count, pinyin, isDefTxt, english']
  where
    isDefTxt = if isDef then "t" else "f"
    count = T.pack $ show frequency
    english' = T.intercalate "/" english


-- /first/second/third/ -> [first, second, third]
splitDefinition :: Text -> [Text]
splitDefinition = filter (not . T.null) . T.splitOn "/" . T.dropAround isSpace
