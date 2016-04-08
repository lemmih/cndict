module Main where

import Data.Chinese.CCDict
import Data.Chinese.Segmentation
--import Data.Chinese.Frequency
import Data.Text
import qualified Data.Text.IO as T
import Control.Exception
import Control.Monad

main :: IO ()
main = do
  putStrLn "Loading dictionary..."
  initiate

  txt <- T.getContents
  putStrLn $ ppTokens $ tokenizer txt
  return ()
