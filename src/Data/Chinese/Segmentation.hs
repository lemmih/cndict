module Data.Chinese.Segmentation where

import qualified Data.Chinese.CCDict as CC
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.IntMap as IntMap

data Block = Block Token [DAG]
type DAG = [Block]

textDAG :: Text -> DAG
textDAG txt = worker [] (T.length txt0 - 1)
  where
    getRelativeDag [] _ = []
    getRelativeDag (dag:_) 0 = dag
    getRelativeDag (_:dags) n = getRelativeDag dags (n-1)
    worker dags n | n<0 = getRelativeDag dag 0
    worker dags n =
      case CC.lookupMatches (T.drop n txt) CC.ccDict of
        Nothing ->
          let this = Block
                        (UnknownWord (T.take 1 $ T.drop n txt))
                        (getRelativeDag dags 1)
              dags' = [this] : dags
          in worker dags' (n-1)
        Just entries ->
          let this = [ Block (KnownW )
                      | entry <- entries ]

lookupMatches :: Text -> CCDict -> Maybe [Entry]
lookup :: Text -> CCDict -> Maybe Entry
