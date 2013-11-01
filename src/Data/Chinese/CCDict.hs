{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-- | Simplified Chinese <-> English dictionary with pinyin phonetics.
module Data.Chinese.CCDict
  ( CCDict
  , Entry(..)
  , load
  , parse
  , lookup
  , ccDict
  , Token(..)
  , tokenizer
  ) where

import           Control.Monad       (mplus)
import           Data.Char
import           Data.FileEmbed
import           Data.List           (foldl', nub)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import qualified Data.Text.IO        as T

import           Prelude             hiding (lookup)

import           Data.Chinese.Pinyin

--------------------------------------------------
-- Dictionary


-- | Dictionary entry
data Entry = Entry
  { entryChinese    :: Text
  , entryPinyin     :: [Text]
  , entryDefinition :: [Text]
  } deriving ( Read, Show, Eq, Ord )

type CCDict = Map Char CCTrieEntry
data CCTrieEntry = CCTrieEntry (Maybe Entry) CCDict

-- | Load dictionary from file.
load :: FilePath -> IO CCDict
load path = parse `fmap` T.readFile path

-- | Load dictionary from unicode text.
parse :: Text -> CCDict
parse txt = fromList [ entry | Just entry <- map parseLine (T.lines txt) ]

-- | O(n). Lookup dictionary entry for a string of simplified chinese.
lookup :: Text -> CCDict -> Maybe Entry
lookup key trie =
    case T.unpack key of
      [] -> Nothing
      (x:xs) -> go xs =<< M.lookup x trie
  where
    go [] (CCTrieEntry es _) = es
    go (x:xs) (CCTrieEntry es m) = (go xs =<< M.lookup x m) `mplus` es




--------------------------------------------------
-- Tokenizer

data Token = KnownWord Entry | UnknownWord Text
  deriving ( Read, Show, Eq, Ord )

-- | Break a string of simplified chinese down to a list of tokens.
tokenizer :: CCDict -> Text -> [Token]
tokenizer trie inp = filter isValid $ go 0 inp inp
  where
    isValid (UnknownWord txt) = not (T.null txt)
    isValid _ = True
    go n unrecognied txt
      | T.null txt = [ unknown ]
      | otherwise =
          case lookup txt trie of
            Nothing -> go (n+1) unrecognied (T.drop 1 txt)
            Just es ->
              let rest = T.drop (T.length (entryChinese es)) txt in
              unknown : KnownWord es : go 0 rest rest
      where
        unknown = UnknownWord $ T.take n unrecognied



--------------------------------------------------
-- Dictionary trie

union :: CCDict -> CCDict -> CCDict
union = M.unionWith join
  where
    join (CCTrieEntry e1 t1) (CCTrieEntry e2 t2) =
      CCTrieEntry (joinEntry e1 e2) (M.unionWith join t1 t2)

joinEntry :: Maybe Entry -> Maybe Entry -> Maybe Entry
joinEntry Nothing Nothing     = Nothing
joinEntry Nothing (Just e)    = Just e
joinEntry (Just e) Nothing    = Just e
joinEntry (Just e1) (Just e2) = Just Entry
  { entryChinese    = entryChinese e1
  , entryPinyin     = entryPinyin e1 -- nub $ entryPinyin e1 ++ entryPinyin e2
  , entryDefinition = nub $ entryDefinition e1 ++ entryDefinition e2 }

unions :: [CCDict] -> CCDict
unions = foldl' union M.empty

fromList :: [Entry] -> CCDict
fromList = unions . map singleton

singleton :: Entry -> CCDict
singleton entry = go (T.unpack (entryChinese entry))
  where
    go []     = error "singleton: Invalid entry."
    go [x]    = M.singleton x (CCTrieEntry (Just entry) M.empty)
    go (x:xs) = M.singleton x (CCTrieEntry Nothing (go xs))

parseLine :: Text -> Maybe Entry
parseLine line | "#" `T.isPrefixOf` line = Nothing
parseLine line =
    Just Entry
    { entryChinese = chinese
    , entryPinyin     = [T.unwords $ map toToneMarks $ T.words $ T.tail $ T.init $ T.unwords (pinyin ++ [pin])]
    , entryDefinition = [T.unwords english] }
  where
    (_traditional : chinese : rest) = T.words line
    (pinyin, (pin : english)) = break (\word -> T.count "]" word > 0) rest



--------------------------------------------------
-- Embedded dictionary

-- | Embedded dictionary.
ccDict :: CCDict
ccDict = parse $ T.decodeUtf8 raw
  where
    raw = $(embedFile "data/cedict_1_0_ts_utf-8_mdbg.txt")

