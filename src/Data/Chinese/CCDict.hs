{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE BangPatterns      #-}
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

import           Control.Monad       (guard)
import           Data.Char
import           Data.FileEmbed
import           Data.List           (foldl')
import           Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict            as M
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
parse txt = fromList $ concat
  [ [ (entrySimplified entry, line), (entryTraditional entry, line) ]
  | line <- T.lines txt
  , Just entry <- [parseLine line] ]

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
lookupNonDet :: Text -> CCDict -> Maybe [[Entry]]
lookupNonDet key trie = do
  entries <- lookupMatches key trie
  let longest = maximum (map (T.length . entrySimplified) entries)
  if longest == 1
    then return [entries]
    else return $ do
      entry <- entries
      let len = T.length (entrySimplified entry)

      case lookupMatches (T.drop len key) trie of
        Just rest | len < longest -> do
          next <- rest
          guard (T.length (entrySimplified next) + len > longest)
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
-- FIXME: 不想 should tokenize to [不,想]
-- FIXME: 那是 should tokenize to [那,是]
-- | Break a string of simplified chinese down to a list of tokens.
tokenizer :: CCDict -> Text -> [Token]
tokenizer = tokenizer'
--tokenizer trie inp = maximumBy (comparing score) (tokenizerNondet trie inp)
-- tokenizer trie inp = filter isValid $ go 0 inp inp
--   where
--     isValid (UnknownWord txt) = not (T.null txt)
--     isValid _ = True
--     go n unrecognied txt
--       | T.null txt = [ unknown ]
--       | otherwise =
--           case lookup txt trie of
--             Nothing -> go (n+1) unrecognied (T.drop 1 txt)
--             Just es ->
--               let rest = T.drop (T.length (entryChinese es)) txt in
--               unknown : KnownWord es : go 0 rest rest
--       where
--         unknown = UnknownWord $ T.take n unrecognied

_tokenizer_tests :: [(Text, [Text], [Text])]
_tokenizer_tests =
    [ (input, result, tokens)
    | (input, result) <- cases
    , let tokens = flat (tokenizer' ccDict input)
    , tokens /= result ]
  where
    cases =
        [ ("多工作", ["多","工作"])
        , ("有电话", ["有","电话"])
      	, ("回电话", ["回","电话"])
      	, ("不知道", ["不","知道"])
      	, ("定时间", ["定","时间"])
      	, ("这位子", ["这","位子"])
      	, ("十分钟", ["十","分钟"])
      	, ("有电梯", ["有","电梯"])
        , ("中午前", ["中午","前"])
        -- , ("得很", ["得","很"]) -- Don't know how to fix this.
        -- , ("家中餐馆", ["家","中餐馆"]) -- tokenizer needs to be more greedy to correctly
                                           -- deal with this input.
      	, ("后生活", ["后","生活"])
      	, ("不愿意", ["不","愿意"])
      	, ("点出发", ["点","出发"])
        , ("不会跳舞", ["不会","跳舞"]) ]

flat :: [Token] -> [Text]
flat tokens = [ entrySimplified entry | KnownWord entry <- tokens ]

type NonDet = Tree [Token]

-- ppNonDet :: [NonDet] -> String
-- ppNonDet forest = drawForest (map (fmap (unwords . map ppToken)) forest)
--   where
--     ppToken (KnownWord entry) = T.unpack (entryChinese entry)
--     ppToken (UnknownWord txt) = T.unpack txt

-- compactNonDet :: NonDet -> NonDet
-- compactNonDet (Node a [Node b rest]) =
--   compactNonDet (Node (a++b) rest)
-- compactNonDet (Node a rest) =
--   Node a (map compactNonDet rest)

collapseNonDet :: [NonDet] -> [Token]
collapseNonDet [] = []
collapseNonDet [Node entries rest] = entries ++ collapseNonDet rest
collapseNonDet (node:nodes) =
    case maxBy nodeScore node nodes of
      Node entries rest -> entries ++ collapseNonDet rest
  where
    maxBy fn x xs = maxBy' (fn x) x xs
      where
        maxBy' _hiScore hiItem [] = hiItem
        maxBy' hiScore hiItem (y:ys) =
          let score = fn y in
          if score > hiScore then maxBy' score y ys else maxBy' hiScore hiItem ys
    geoMean :: [Int] -> Double
    geoMean [] = 0
    geoMean n = fromIntegral (product n)**(recip (fromIntegral (length n)))
    -- assocs = [ (node, geoMean (filter (/=0) (nodeSum node)))
    --          | node <- forest ]
    wordCount word = maybe 0 subtlexWCount (M.lookup word subtlex)
    entryCount (KnownWord entry) = wordCount (entrySimplified entry)
    entryCount UnknownWord{} = 0
    nodeSum (Node entries _) = map entryCount entries
    nodeScore = geoMean . filter (/=0) . nodeSum

-- Enhanced tokenizer, mixed non-determistic and greedy algorithm
tokenizer' :: CCDict -> Text -> [Token]
tokenizer' trie inp = collapseNonDet (tokenizerNondet trie inp)

tokenizerNondet :: CCDict -> Text -> [NonDet]
tokenizerNondet trie inp = go inp
  where
    go txt | T.null txt = []
    go txt =
      case lookupNonDet txt trie of
        Nothing -> do
          return $ Node [UnknownWord (T.take 1 txt)] $ go (T.drop 1 txt)
        Just es -> do
          entries <- es
          let len = sum (map (T.length . entrySimplified) entries)
          return $ Node (map KnownWord entries) $ go (T.drop len txt)

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

-- union :: CCDict -> CCDict -> CCDict
-- union = IntMap.unionWith joinTrie

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
joinRawEntry e1 e2 = T.unlines [e1,e2]

joinEntry :: Entry -> Entry -> Entry
joinEntry e1 e2 = Entry
  { entrySimplified  = entrySimplified e1
  , entryTraditional = entryTraditional e1
  , entryPinyin      = entryPinyin e1 ++ entryPinyin e2
  , entryDefinition  = entryDefinition e1 ++ entryDefinition e2 }

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

-- | Embedded dictionary.
ccDict :: CCDict
ccDict = parse $ T.decodeUtf8 raw
  where
    raw = $(embedFile "data/cedict_1_0_ts_utf-8_mdbg.txt")

-- ccDict' :: CCDict
-- ccDict' = decode (BL.fromStrict raw)
--   where
--     raw = $(embedFile "data/cedict_1_0_ts_utf-8_mdbg.txt.binary")
