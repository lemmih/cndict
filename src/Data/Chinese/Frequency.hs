{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Data.Chinese.Frequency
  ( FreqMap
  , freqMap
  ) where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import           Data.Map              (Map)
import qualified Data.Map.Strict       as M
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import qualified Data.Text.Read        as T
import           Data.Text.Encoding
import           Paths_cndict
import           System.IO.Unsafe      (unsafePerformIO)

type FreqMap = Map Text Int

freqMap :: FreqMap
freqMap = mkFreqMap rows
  where
    utfData = unsafePerformIO $ do
      path  <- getDataFileName "data/dict.txt.big"
      T.readFile path
    rows = T.lines utfData

mkFreqMap :: [Text] -> FreqMap
mkFreqMap rows = M.fromListWith max
      [ (word, count)
      | (n,row) <- zip [0..] rows
      , let [word,countStr,_type] = T.words row
            Right (count,_) = T.decimal countStr
      ]
