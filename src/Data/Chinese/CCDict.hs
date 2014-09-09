{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE CPP   #-}
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
  , tokenizer'
  ) where

import           Control.Monad       (mplus,guard)
import           Data.Char
import           Data.FileEmbed
import           Data.List           (foldl', nub, maximumBy, sortBy)
import Data.Ord (comparing)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import qualified Data.Text.IO        as T

import           Prelude             hiding (lookup)

import Data.Tree

import           Data.Chinese.Pinyin
import           Data.Chinese.Frequency

--------------------------------------------------
-- Dictionary


-- | Dictionary entry
data Entry = Entry
  { entryChinese    :: Text
  , entryPinyin     :: [Text]
  , entryDefinition :: [[Text]]
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

lookupMatches :: Text -> CCDict -> Maybe [Entry]
lookupMatches key trie =
    case T.unpack key of
      []     -> Nothing
      (x:xs) ->
        case fmap (go xs) (M.lookup x trie) of
          Just [] -> Nothing
          other   -> other
  where
    go [] (CCTrieEntry Nothing _) = []
    go [] (CCTrieEntry (Just e) _) = [e]
    go (x:xs) (CCTrieEntry Nothing m) = maybe [] (go xs) (M.lookup x m)
    go (x:xs) (CCTrieEntry (Just e) m) = e : maybe [] (go xs) (M.lookup x m)


-- 点出发
-- [[点,出发],[点出,发]]
-- 出发点
-- [[出发]]
lookupNonDet :: Text -> CCDict -> Maybe [[Entry]]
lookupNonDet key trie = do
  entries <- lookupMatches key trie
  let longest = maximum (map (T.length . entryChinese) entries)
  if longest == 1
    then return [entries]
    else return $ do
      entry <- entries
      let len = T.length (entryChinese entry)

      case lookupMatches (T.drop len key) trie of
        Just rest | len < longest -> do
          next <- rest
          guard (T.length (entryChinese next) + len > longest)
          return [entry, next]
        _nothing -> return [entry]


--------------------------------------------------
-- Tokenizer

data Token = KnownWord Entry | UnknownWord Text
  deriving ( Read, Show, Eq, Ord )

-- Interesting case: 他的话 tokenizes to [他,的话] by both google translate and
-- MDGB. The correct tokenization is [他,的,话]. Not sure if it can be fixed without
-- adding an entry for 他的 in the dictionary.
-- TODO: Mark text inclosed in curly brackets as unknown words.
-- FIXME: 多工作 should tokenize to [多,工作]
-- FIXME: 不想 should tokenize to [不,想]
-- FIXME: 回电话 should tokenize to [回,电话]
-- FIXME: 不知道 should tokenize to [不,知道]
-- FIXME: 定时间 should tokenize to [定,时间]
-- FIXME: 这位子 should tokenize to [这,位子]
-- FIXME: 十分钟 should tokenize to [十,分钟]
-- FIXME: 有电梯 should tokenize to [有,电梯]
-- FIXME: 家中餐馆 should tokenize to [家,中餐馆]
-- FIXME: 那是 should tokenize to [那,是]
-- FIXME: 后生活 should tokenize to [后,生活]
-- FIXME: 不愿意 should tokenize to [不,愿意]
-- FIXME: 点出发 should tokenize to [点,出发]
-- | Break a string of simplified chinese down to a list of tokens.
tokenizer :: CCDict -> Text -> [Token]
--tokenizer trie inp = maximumBy (comparing score) (tokenizerNondet trie inp)
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

tokenizer_tests :: [(Text, [Text], [Text])]
tokenizer_tests =
    [ (input, result, tokens)
    | (input, result) <- cases
    , let tokens = flat (tokenizer' ccDict input)
    , tokens /= result ]
  where
    cases =
        [ ("多工作", ["多","工作"])
        , ("有电话", ["有","电话"]) 
      	-- , ("回电话", ["回","电话"]) -- Hm, we fail on this one.
      	, ("不知道", ["不","知道"])
      	, ("定时间", ["定","时间"])
      	-- , ("这位子", ["这","位子"]) -- Hm, we fail on this one.
      	, ("十分钟", ["十","分钟"])
      	, ("有电梯", ["有","电梯"])
              -- , ("家中餐馆", ["家","中餐馆"])
      	, ("后生活", ["后","生活"])
      	, ("不愿意", ["不","愿意"])
      	, ("点出发", ["点","出发"])
        , ("不会跳舞", ["不会","跳舞"]) ]

flat tokens = [ entryChinese entry | KnownWord entry <- tokens ]

type NonDet = Tree [Token]

ppNonDet :: [NonDet] -> String
ppNonDet forest = drawForest (map (fmap (unwords . map ppToken)) forest)
  where
    ppToken (KnownWord entry) = T.unpack (entryChinese entry)
    ppToken (UnknownWord txt) = T.unpack txt

compactNonDet :: NonDet -> NonDet
compactNonDet (Node a [Node b rest]) =
  compactNonDet (Node (a++b) rest)
compactNonDet (Node a rest) =
  Node a (map compactNonDet rest)

collapseNonDet :: [NonDet] -> [Token]
collapseNonDet forest =
    case listToMaybe (sortBy (flip $ comparing snd) assocs) of
      Nothing -> []
      Just (Node entries rest,score) -> entries ++ collapseNonDet rest
  where
    assocs = [ (node, nodeSum node)
             | node <- forest ]
    wordCount word = maybe 0 subtlexWCount (M.lookup word subtlex)
    entryCount (KnownWord entry) = wordCount (entryChinese entry)
    entryCount UnknownWord{} = 0
    entriesSum = sum . map entryCount
    nodeSum (Node entries _) = entriesSum entries

-- Enhanced tokenizer, mixed non-determistic and greedy algorithm
tokenizer' :: CCDict -> Text -> [Token]
tokenizer' trie inp = collapseNonDet (tokenizerNondet trie inp)

tokenizerNondet :: CCDict -> Text -> [NonDet]
tokenizerNondet trie inp = go inp
  where
    isValid (UnknownWord txt) = not (T.null txt)
    isValid _ = True
    go txt | T.null txt = []
    go txt =
      case lookupNonDet txt ccDict of
        Nothing -> do
          --rest <- go (T.drop 1 txt)
          --return (UnknownWord (T.take 1 txt) : rest)
          return $ Node [UnknownWord (T.take 1 txt)] $ go (T.drop 1 txt)
        Just es -> do
          entries <- es
          let len = sum (map (T.length . entryChinese) entries)
          return $ Node (map KnownWord entries) $ go (T.drop len txt)
          --rest  <- go (T.drop len txt)
          --return (map KnownWord entries ++ rest)
      --case lookupExact word trie of
      --  Nothing -> do
      --    guard (len == 1)
      --    rest <- go (T.drop len txt)
      --    return (UnknownWord word : rest)
      --  Just es -> do
      --    rest <- go (T.drop len txt)
      --    return (KnownWord es : rest)

--score :: [Token] -> Double
--score = sum . map fn
--  where
--    fn UnknownWord{} = 0
--    fn (KnownWord entry) | T.length (entryChinese entry) == 1 = 0
--    fn (KnownWord entry) =
--      case M.lookup (entryChinese entry) subtlex of
--        Nothing   -> 0
--        Just freq -> subtlexWMillion freq


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
  , entryPinyin     = entryPinyin e1 ++ entryPinyin e2
  , entryDefinition = entryDefinition e1 ++ entryDefinition e2 }

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
    { entryChinese    = chinese
    , entryPinyin     = [T.unwords $ map toToneMarks $ T.words $ T.tail $ T.init $ T.unwords (pinyin ++ [pin])]
    , entryDefinition = [splitDefinition (T.unwords english)] }
  where
    (_traditional : chinese : rest) = T.words line
    (pinyin, (pin : english)) = break (\word -> T.count "]" word > 0) rest

-- /first/second/third/ -> [first, second, third]
splitDefinition :: Text -> [Text]
splitDefinition = filter (not . T.null) . T.splitOn "/"


--------------------------------------------------
-- Embedded dictionary

-- | Embedded dictionary.
ccDict :: CCDict
ccDict = parse $ T.decodeUtf8 raw
  where
    raw = $(embedFile "data/cedict_1_0_ts_utf-8_mdbg.txt")
