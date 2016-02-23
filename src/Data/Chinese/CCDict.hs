{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Simplified Chinese <-> English dictionary with pinyin phonetics.
module Data.Chinese.CCDict
  ( CCDict
  , Entry(..)
  , load
  , parse
  , lookup
  , lookupMatches
  , ccDict
  ) where

import qualified Data.ByteString        as B
import           Data.Char
import           Data.IntMap            (IntMap)
import qualified Data.IntMap.Strict     as IntMap
import           Data.List              (foldl', maximumBy, nub)
import           Data.Maybe
import           Data.Ord
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.Text.IO           as T
import           Paths_cndict
import           Prelude                hiding (lookup)
import           System.IO.Unsafe       (unsafePerformIO)

import           Data.Tree

import qualified Data.Chinese.Frequency as Frequency
import           Data.Chinese.Pinyin

import           Data.Chinese.Frequency hiding (lookup)

--------------------------------------------------
-- Dictionary


-- | Dictionary entry
data Entry = Entry
  { entrySimplified  :: !Text
  , entryTraditional :: !Text
  , entryPinyin      :: [Text]
  , entryDefinition  :: [[Text]]
  } deriving ( Read, Show, Eq, Ord )

type RawEntry = Text

-- entryPinyin :: Entry -> [Text]
-- entryPinyin = map (T.unwords . map toToneMarks . T.words) . entryPinyinRaw

type CCDict = IntMap CCTrieEntry
data CCTrieEntry
  = CCTrieEntry    {-# UNPACK #-} !RawEntry !CCDict
  | CCTrieEntryEnd {-# UNPACK #-} !RawEntry
  | CCTrieNoEntry                           !CCDict
  deriving ( Show )


-- instance Binary CCTrieEntry where
--   put (CCTrieEntry entry rest) = put entry >> put rest
--   get = CCTrieEntry <$> get <*> get

-- | Load dictionary from file.
load :: FilePath -> IO CCDict
load path = parse `fmap` T.readFile path

-- | Load dictionary from unicode text.
parse :: Text -> CCDict
parse txt = fromList
  [ (key, line)
  | line <- T.lines txt
  , Just entry <- [parseLine line]
  , key <- nub [entrySimplified entry, entryTraditional entry] ]

-- | O(n). Lookup dictionary entry for a string of simplified chinese.
lookup :: Text -> CCDict -> Maybe Entry
lookup key trie =
    case map ord $ T.unpack key of
      [] -> Nothing
      (x:xs) -> fmap parseRawEntry (go xs =<< IntMap.lookup x trie)
  where
    go _ (CCTrieEntryEnd es) = Just es
    go [] (CCTrieEntry es _) = Just es
    go [] (CCTrieNoEntry _) = Nothing
    go (x:xs) (CCTrieEntry es m) = Just (fromMaybe es (go xs =<< IntMap.lookup x m))
    go (x:xs) (CCTrieNoEntry m) = (go xs =<< IntMap.lookup x m)

lookupMatches :: Text -> CCDict -> Maybe [Entry]
lookupMatches key trie =
    case map ord $ T.unpack key of
      []     -> Nothing
      (x:xs) ->
        case fmap (map parseRawEntry . go xs) (IntMap.lookup x trie) of
          Just [] -> Nothing
          other   -> other
  where
    go _ (CCTrieEntryEnd e) = [e]
    go [] (CCTrieNoEntry _) = []
    go [] (CCTrieEntry e _) = [e]
    go (x:xs) (CCTrieNoEntry m) = maybe [] (go xs) (IntMap.lookup x m)
    go (x:xs) (CCTrieEntry e m) = e : maybe [] (go xs) (IntMap.lookup x m)


-- 点出发
-- [[点,出发],[点出,发]]
-- 出发点
-- [[出发]]
-- 穿上外套
-- This can be broken up in two ways: 穿 上外 套 and 穿上 外套
-- We want the second, more greedy tokenization.
lookupNonDet :: Text -> CCDict -> Maybe [[Entry]]
lookupNonDet key trie = do
    entries <- lookupMatches key trie
    let longest = maximumBy (comparing (T.length . entrySimplified)) entries

    if length entries == 1
      then return [entries]
      else do
        return $ whenEmpty [[longest]] $ maybe [] beGreedy $ sequence $ do
          entry1 <- entries
          case lookupMatches (T.drop (T.length (entrySimplified entry1)) key) trie of
            Nothing -> return Nothing
            Just entries2 -> do
              entry2 <- entries2
              return $ Just (entry1, entry2)
  where
    filterCompact :: [[Entry]] -> [[Entry]]
    filterCompact lst =
      let mostCompact = minimum (map length lst)
      in [ entries | entries <- lst, length entries == mostCompact ]
    filterLongest :: [[Entry]] -> [[Entry]]
    filterLongest lst =
      let len = sum . map (T.length . entrySimplified)
          longest = maximum (map len lst)
      in [ entries | entries <- lst, len entries == longest ]
    beGreedy :: [(Entry,Entry)] -> [[Entry]]
    beGreedy lst =
      let longestFirst = maximum (map (T.length . entrySimplified . fst) lst)
          longest = maximum [ T.length (entrySimplified e1) + T.length (entrySimplified e2)
                    | (e1,e2) <- lst
                    , T.length (entrySimplified e1) < longestFirst ]
      in filterCompact $ filterLongest $ nub $
         [ [e1,e2]
         | (e1,e2) <- lst
         , T.length (entrySimplified e1) < longest
         , T.length (entrySimplified e1) + T.length (entrySimplified e2) /= 2 ] ++
         [ [e1]
         | (e1,_) <- lst
         , T.length (entrySimplified e1) == longest ]
    whenEmpty lst [] = lst
    whenEmpty _ lst  = lst
  --   step Nothing _ = []
  --   step (Just [x]) _ = return [x]
  --   step (Just lst) fn = lst >>= fn
  --   beGreedy lst =
  --     let len = sum . map (T.length . entrySimplified)
  --         longest = maximum (map len lst')
  --         mostCompact = minimum (map length lst)
  --         lst' = filter (\x -> length x == mostCompact) lst
  --     in filter (\x -> len x == longest) lst'
  --   toMaybe [] = Nothing
  --   toMaybe lst = Just lst

--------------------------------------------------
-- Tokenizer


-- Interesting case: 他的话 tokenizes to [他,的话] by both google translate and
-- MDGB. The correct tokenization is [他,的,话]. Not sure if it can be fixed without
-- adding an entry for 他的 in the dictionary.
-- TODO: Mark text inclosed in curly brackets as unknown words.
-- FIXME: 不想 should tokenize to [不,想]
-- FIXME: 那是 should tokenize to [那,是]


--------------------------------------------------
-- Dictionary trie

-- union :: CCDict -> CCDict -> CCDict
-- union = IntMap.unionWith joinTrie

-- joinTrie newValue oldValue
joinTrie :: CCTrieEntry -> CCTrieEntry -> CCTrieEntry
joinTrie (CCTrieNoEntry t1) (CCTrieNoEntry t2) = CCTrieNoEntry (IntMap.unionWith joinTrie t1 t2)
joinTrie (CCTrieNoEntry t1) (CCTrieEntry e t2) = CCTrieEntry e (IntMap.unionWith joinTrie t1 t2)
joinTrie (CCTrieNoEntry t1) (CCTrieEntryEnd e) = CCTrieEntry e t1
joinTrie (CCTrieEntry e t1) (CCTrieNoEntry t2) = CCTrieEntry e (IntMap.unionWith joinTrie t1 t2)
joinTrie (CCTrieEntry e1 t1) (CCTrieEntry e2 t2) =
  CCTrieEntry (joinRawEntry e1 e2) (IntMap.unionWith joinTrie t1 t2)
joinTrie (CCTrieEntry e1 t2) (CCTrieEntryEnd e2) = CCTrieEntry (joinRawEntry e1 e2) t2
joinTrie (CCTrieEntryEnd e) (CCTrieNoEntry t)    = CCTrieEntry e t
joinTrie (CCTrieEntryEnd e1) (CCTrieEntry e2 t)  = CCTrieEntry (joinRawEntry e1 e2) t
joinTrie (CCTrieEntryEnd e1) (CCTrieEntryEnd e2) = CCTrieEntryEnd (joinRawEntry e1 e2)

joinRawEntry :: RawEntry -> RawEntry -> RawEntry
joinRawEntry e1 e2 = T.concat [e1, "\n", e2]

-- joinEntry newValue oldValue
joinEntry :: Entry -> Entry -> Entry
joinEntry e1 e2 = Entry
  { -- The simplified characters must be identical
    entrySimplified  = entrySimplified e1
    -- 了 maps to two traditional characters: 了 and 瞭.
    -- In these cases, choose the same as the simplified.
  , entryTraditional =
    if entryTraditional e1 == entrySimplified e1 ||
       entryTraditional e2 == entrySimplified e1
       then entrySimplified e1
       else entryTraditional e1
  , entryPinyin      = entryPinyin e2 ++ entryPinyin e1
  , entryDefinition  = entryDefinition e2 ++ entryDefinition e1 }

-- unions :: [CCDict] -> CCDict
-- unions = foldl' union IntMap.empty

fromList :: [(Text, RawEntry)] -> CCDict
-- fromList = unions . map singleton
fromList = foldl' (flip insert) IntMap.empty

insert :: (Text, RawEntry) -> CCDict -> CCDict
insert (key, entry) = go (T.unpack key)
  where
    go :: [Char] -> CCDict -> CCDict
    go [] _ = error "insert: Invalid entry."
    go [x] t =
      IntMap.insertWith joinTrie (ord x) (CCTrieEntryEnd entry) t
    go (x:xs) t =
      IntMap.alter (go' xs) (ord x) t
    go' xs Nothing = Just $ CCTrieNoEntry (go xs IntMap.empty)
    go' xs (Just trie) = Just $
      case trie of
        CCTrieNoEntry t -> CCTrieNoEntry $ go xs t
        CCTrieEntry e t -> CCTrieEntry e $ go xs t
        CCTrieEntryEnd e -> CCTrieEntry e $ go xs IntMap.empty

-- singleton :: Entry -> CCDict
-- singleton entry = go (T.unpack (entryChinese entry))
--   where
--     go []     = error "singleton: Invalid entry."
--     go [x]    = IntMap.singleton (ord x) (CCTrieEntryEnd entry)
--     go (x:xs) = IntMap.singleton (ord x) (CCTrieNoEntry (go xs))

parseRawEntry :: Text -> Entry
parseRawEntry = foldr1 joinEntry . mapMaybe parseLine . T.lines

parseLine :: Text -> Maybe Entry
parseLine line | "#" `T.isPrefixOf` line = Nothing
parseLine line =
    Just Entry
    { entrySimplified  = simplified
    , entryTraditional = traditional
    , entryPinyin      = [T.unwords $ map toToneMarks $ T.words pinyin]
    , entryDefinition  = [splitDefinition english] }
    -- , entryPinyin     = V.singleton $ T.unwords $ map toToneMarks $ T.words $ T.tail $
    --                       T.init $ T.unwords (pinyin ++ [pin])
    -- , entryDefinition = V.singleton $ splitDefinition (T.unwords english) }
  where
    (traditional, line') = T.breakOn " " line
    (simplified, line'') = T.breakOn " " (T.drop 1 line')
    (pinyin_, english_) = T.breakOn "/" (T.drop 1 line'')
    !english = english_
    !pinyin = T.dropAround (\c -> isSpace c || c == '[' || c == ']') pinyin_
    -- firstSep = breakOn " ", breakOn " ", breakOn "/"
    -- (_traditional : chinese : rest) = T.words (T.copy line)
    -- (pinyin, (pin : english)) = break (\word -> T.count "]" word > 0) rest

-- /first/second/third/ -> [first, second, third]
splitDefinition :: Text -> [Text]
splitDefinition = filter (not . T.null) . T.splitOn "/" . T.dropAround isSpace




--------------------------------------------------
-- Embedded dictionary

remove :: Text -> CCDict -> CCDict
remove = worker . map ord . T.unpack
  where
    worker [] dict = dict
    worker (x:xs) dict =
      IntMap.update (fn xs) x dict
    fn xs (CCTrieNoEntry rest)  = Just $ CCTrieNoEntry (worker xs rest)
    fn [] CCTrieEntryEnd{}      = Nothing
    fn _ (CCTrieEntryEnd entry) = Just $ CCTrieEntryEnd entry
    fn [] (CCTrieEntry _ rest)  = Just $ CCTrieNoEntry rest
    fn xs (CCTrieEntry e rest)  = Just $ CCTrieEntry e (worker xs rest)

-- | Embedded dictionary.
ccDict :: CCDict
ccDict =
    remove "得很" $
    remove "那是" $ remove "到了" $
    remove "里人" $ remove "多事" $
    remove "你我" $ remove "家的" $
    parse $ T.decodeUtf8 raw
  where
    -- raw = $(embedFile "data/cedict_1_0_ts_utf-8_mdbg.txt")
    raw = unsafePerformIO $ do
      path  <- getDataFileName "data/cedict_1_0_ts_utf-8_mdbg.txt"
      B.readFile path

-- ccDict' :: CCDict
-- ccDict' = decode (BL.fromStrict raw)
--   where
--     raw = $(embedFile "data/cedict_1_0_ts_utf-8_mdbg.txt.binary")
