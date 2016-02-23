module Main where

import Data.Chinese.CCDict
import Data.Chinese.Segmentation
import Data.Chinese.Frequency
import Data.Text
import Control.Exception
import Control.Monad

main :: IO ()
main = do
  putStrLn "Loading dictionary..."
  evaluate ccDict
  putStrLn "Loading word frequency database..."
  evaluate freqMap
  forever $ do
    putStrLn "Type some Chinese characters and I'll tell you how to segment the words:"
    txt <- getLine
    putStrLn $ ppTokens $ tokenizer ccDict (pack txt)
