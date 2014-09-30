{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Data.Chinese.Frequency
  ( SubtlexMap
  , SubtlexEntry(..)
  , subtlex
  ) where

import           Control.Applicative
import qualified Data.ByteString.Lazy as L
import           Data.Csv             as Csv
import           Data.FileEmbed
import           Data.Map             (Map)
import qualified Data.Map.Strict            as M
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Vector          (Vector)
import qualified Data.Vector          as V

-- import           Data.Chinese.Pinyin

type SubtlexMap = Map Text SubtlexEntry

data SubtlexEntry = SubtlexEntry
  { subtlexIndex    :: {-# UNPACK #-} !Int
  , subtlexWord     :: {-# UNPACK #-} !T.Text
  -- , subtlexPinyin   :: [T.Text]
  , subtlexWCount   :: {-# UNPACK #-} !Int
  , subtlexWMillion :: {-# UNPACK #-} !Double
  -- , subtlexEnglish  :: T.Text
  } deriving ( Show )

instance FromRecord SubtlexEntry where
  parseRecord rec = SubtlexEntry
    <$> pure 0
    <*> fmap T.copy (index rec 0)
    -- <*> fmap (map toToneMarks . T.splitOn "/") (index rec 2)
    <*> index rec 4
    <*> index rec 5
    -- <*> index rec 14

_loadSubtlexEntries :: FilePath -> IO (Vector SubtlexEntry)
_loadSubtlexEntries path = do
  inp <- L.readFile path
  case Csv.decodeWith (Csv.DecodeOptions 9) HasHeader inp of
    Left msg   -> error msg
    Right rows -> return rows

mkSubtlexMap :: Vector SubtlexEntry -> SubtlexMap
mkSubtlexMap rows = M.fromListWith join
  [ (subtlexWord row, row{subtlexIndex = n})
  | (n,row) <- zip [0..] (V.toList rows)
  -- , subtlexEnglish row /= "#"
  ]
  where
    join e1 e2 = SubtlexEntry
      { subtlexIndex = min (subtlexIndex e1) (subtlexIndex e2)
      , subtlexWord  = subtlexWord e1
      -- , subtlexPinyin = subtlexPinyin e1
      , subtlexWCount = subtlexWCount e1 + subtlexWCount e2
      , subtlexWMillion = subtlexWMillion e1 + subtlexWMillion e2
      -- , subtlexEnglish = subtlexEnglish e1
      }





------------------------------------------------------------
-- Embedded files

subtlex :: SubtlexMap
subtlex = mkSubtlexMap $
  case Csv.decodeWith (Csv.DecodeOptions 9) HasHeader inp of
    Left msg -> error msg
    Right rows -> rows
  where
    inp = L.fromStrict $(embedFile "data/SUBTLEX_CH_131210_CE.utf8")
