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
import qualified Data.Map             as M
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Vector          (Vector)
import qualified Data.Vector          as V

import           Data.Chinese.Pinyin

type SubtlexMap = Map Text SubtlexEntry

data SubtlexEntry = SubtlexEntry
  { subtlexWord     :: T.Text
  , subtlexPinyin   :: [T.Text]
  , subtlexWCount   :: Int
  , subtlexWMillion :: Double
  , subtlexEnglish  :: T.Text
  } deriving ( Show )

instance FromRecord SubtlexEntry where
  parseRecord rec = SubtlexEntry
    <$> index rec 0
    <*> fmap (map toToneMarks . T.splitOn "/") (index rec 2)
    <*> index rec 4
    <*> index rec 5 <*> index rec 14

loadSubtlexEntries :: FilePath -> IO (Vector SubtlexEntry)
loadSubtlexEntries path = do
  inp <- L.readFile path
  case Csv.decodeWith (Csv.DecodeOptions 9) True inp of
    Left msg   -> error msg
    Right rows -> return rows

mkSubtlexMap :: Vector SubtlexEntry -> SubtlexMap
mkSubtlexMap rows = M.fromList [ (subtlexWord row, row) | row <- V.toList rows ]






------------------------------------------------------------
-- Embedded files

subtlex :: SubtlexMap
subtlex = mkSubtlexMap $
  case Csv.decodeWith (Csv.DecodeOptions 9) True inp of
    Left msg -> error msg
    Right rows -> rows
  where
    inp = L.fromStrict $(embedFile "data/SUBTLEX_CH_131210_CE.utf8")
