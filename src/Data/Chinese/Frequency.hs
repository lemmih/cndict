{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Data.Chinese.Frequency
  ( SubtlexMap
  , SubtlexEntry(..)
  , subtlex
  , Data.Chinese.Frequency.lookup
  ) where

-- import           Control.Applicative
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString      as B
import qualified Data.ByteString.Char8 as B8
-- import           Data.Csv             as Csv
import           Data.FileEmbed
import           Data.Map             (Map)
import qualified Data.Map.Strict            as M
import           Data.Text            (Text)
import           Data.Text.Encoding
import qualified Data.Text            as T
-- import           Data.Vector          (Vector)
-- import qualified Data.Vector          as V

-- import           Data.Chinese.Pinyin

type SubtlexMap = Map B.ByteString RawEntry

data RawEntry = RawEntry
  { rawEntryIndex    :: {-# UNPACK #-} !Int
  , rawEntryWCount   :: {-# UNPACK #-} !Int
  , rawEntryWMillion :: {-# UNPACK #-} !Double
  }

data SubtlexEntry = SubtlexEntry
  { subtlexIndex    :: !Int
  , subtlexWord     :: !T.Text
  , subtlexWCount   :: !Int
  , subtlexWMillion :: !Double
  } deriving ( Show )

toEntry :: Int -> B.ByteString -> RawEntry
toEntry idx row = RawEntry
    { rawEntryIndex    = idx
    , rawEntryWCount   = asInt (chunks!!4)
    , rawEntryWMillion = read (B8.unpack $ chunks!!5) }
  where
    chunks = B.split 9 row
    asInt str =
      case B8.readInt str of
        Nothing        -> -1
        Just (n,_rest) -> n

lookup :: Text -> SubtlexMap -> Maybe SubtlexEntry
lookup key m = do
    RawEntry n wcount wmillion <- M.lookup (encodeUtf8 key) m
    return SubtlexEntry
      { subtlexIndex = n
      , subtlexWord  = key
      , subtlexWCount = wcount
      , subtlexWMillion = wmillion }
-- instance FromRecord SubtlexEntry where
--   parseRecord rec = SubtlexEntry
--     <$> pure 0
--     <*> fmap T.copy (index rec 0)
--     -- <*> fmap (map toToneMarks . T.splitOn "/") (index rec 2)
--     <*> index rec 4
--     <*> index rec 5
--     -- <*> index rec 14

-- _loadSubtlexEntries :: FilePath -> IO (Vector SubtlexEntry)
-- _loadSubtlexEntries path = do
--   inp <- L.readFile path
--   case Csv.decodeWith (Csv.DecodeOptions 9) HasHeader inp of
--     Left msg   -> error msg
--     Right rows -> return rows

-- mkSubtlexMap :: Vector SubtlexEntry -> SubtlexMap
-- mkSubtlexMap rows = M.fromListWith join
--   [ (subtlexWord row, row{subtlexIndex = n})
--   | (n,row) <- zip [0..] (V.toList rows)
--   -- , subtlexEnglish row /= "#"
--   ]
--   where
--     join e1 e2 = SubtlexEntry
--       { subtlexIndex = min (subtlexIndex e1) (subtlexIndex e2)
--       , subtlexWord  = subtlexWord e1
--       -- , subtlexPinyin = subtlexPinyin e1
--       , subtlexWCount = subtlexWCount e1 + subtlexWCount e2
--       , subtlexWMillion = subtlexWMillion e1 + subtlexWMillion e2
--       -- , subtlexEnglish = subtlexEnglish e1
--       }

mkSubtlexMap :: [B.ByteString] -> SubtlexMap
mkSubtlexMap rows = M.fromListWith join
  [ (word, toEntry n row)
  | (n,row) <- zip [0..] rows
  , let chunks = B.split 9 row
        word = head chunks
  , not (null chunks)
  -- , subtlexEnglish row /= "#"
  ]
  where
    join (RawEntry n1 c1 m1) (RawEntry n2 c2 m2) =
      RawEntry (min n1 n2) (c1+c2) (m1+m2)




------------------------------------------------------------
-- Embedded files

subtlex :: SubtlexMap
subtlex = mkSubtlexMap $
    rows
  where
    utfData = $(embedFile "data/SUBTLEX_CH_131210_CE.utf8")
    rows = drop 1 (B.split 0xa utfData)

-- subtlex :: SubtlexMap
-- subtlex = mkSubtlexMap $
--   case Csv.decodeWith (Csv.DecodeOptions 9) HasHeader inp of
--     Left msg -> error msg
--     Right rows -> rows
--   where
--     inp = L.fromStrict $(embedFile "data/SUBTLEX_CH_131210_CE.utf8")

