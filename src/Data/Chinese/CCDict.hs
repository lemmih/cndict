{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-- | Simplified Chinese <-> English dictionary with pinyin phonetics.
module Data.Chinese.CCDict
  ( CCDict
  , Entry(..)
  , load
  , parse
  , lookup
  , lookupMatches
  , ccDict
  , Token(..)
  , tokenizer
  , toTraditional
  , toSimplified
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

_ppTokenizerTests :: IO ()
_ppTokenizerTests =
  case _tokenizer_tests of
    [] -> putStrLn "No test failures."
    lst -> do
      flip mapM_ lst $ \(orig, expected, actual) -> do
        T.putStr orig
        putStr ": expected: "
        T.putStr (T.unwords expected)
        putStr ", got: "
        T.putStrLn (T.unwords actual)

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
        , ("好心地", ["好心","地"])
        -- , ("得很", ["得","很"])
        -- , ("不想", ["不","想"])
        -- , ("那是", ["那","是"])
        , ("外套", ["外套"])
        , ("家中餐馆", ["家","中餐馆"])
        , ("后生活", ["后","生活"])
        , ("不愿意", ["不","愿意"])
        , ("点出发", ["点","出发"])
        , ("老婆婆", ["老","婆婆"])
        , ("不会跳舞", ["不会","跳舞"])
        , ("穿上外套", ["穿上","外套"])
        , ("建议", ["建议"])
        , ("怎么不知道", ["怎么","不","知道"])
        , ("蛋糕发起来", ["蛋糕","发","起来"])
        , ("管理的人才", ["管理","的","人才"])
        , ("轻快乐曲", ["轻快","乐曲"])
        , ("高明和", ["高明","和"])
        , ("一下子之间", ["一下子","之间"])
        , ("我绝没想到", ["我","绝","没想到"])
        , ("没想到会", ["没想到","会"]) ]

flat :: [Token] -> [Text]
flat = map worker
  where
    worker (KnownWord entry) = entrySimplified entry
    worker (UnknownWord txt) = txt

type NonDet = Tree [Token]

_ppNonDet :: [NonDet] -> String
_ppNonDet = drawForest . map (fmap (unwords . map ppToken))
  where
    ppToken (KnownWord entry) = T.unpack (entrySimplified entry)
    ppToken (UnknownWord txt) = T.unpack txt

_compactNonDet :: NonDet -> NonDet
_compactNonDet (Node a [Node b rest]) =
  _compactNonDet (Node (a++b) rest)
_compactNonDet (Node a rest) =
  Node a (map _compactNonDet rest)

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
    geoMean :: [Int] -> Integer
    geoMean [] = 0
    geoMean n = product $ map fromIntegral n
    -- assocs = [ (node, geoMean (filter (/=0) (nodeSum node)))
    --          | node <- forest ]
    wordCount word = maybe 1 subtlexWCount (Frequency.lookup word subtlex)
    entryCount (KnownWord entry) = wordCount (entrySimplified entry)
    entryCount UnknownWord{} = 1
    nodeSum (Node entries _) = map entryCount entries
    nodeScore = geoMean . nodeSum

-- Enhanced tokenizer, mixed non-determistic and greedy algorithm
tokenizer' :: CCDict -> Text -> [Token]
tokenizer' trie inp = compress $ collapseNonDet (tokenizerNondet trie inp)
  where
    compress [] = []
    compress (UnknownWord a:UnknownWord b:xs) = compress (UnknownWord (a `T.append` b):xs)
    compress (x:xs) = x:compress xs

tokenizerNondet :: CCDict -> Text -> [NonDet]
tokenizerNondet trie inp = map _compactNonDet $ go inp
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
-- Simplified <-> Traditional

flatMap :: (Entry -> Text) -> [Token] -> Text
flatMap fn = T.concat . map worker
  where
    worker (KnownWord e)     = fn e
    worker (UnknownWord txt) = txt

toTraditional :: Text -> Text
toTraditional = flatMap entryTraditional . tokenizer ccDict

toSimplified :: Text -> Text
toSimplified = flatMap entrySimplified . tokenizer ccDict

--------------------------------------------------
-- Embedded dictionary

-- | Embedded dictionary.
ccDict :: CCDict
ccDict = parse $ T.decodeUtf8 raw
  where
    -- raw = $(embedFile "data/cedict_1_0_ts_utf-8_mdbg.txt")
    raw = unsafePerformIO $ do
      path  <- getDataFileName "data/cedict_1_0_ts_utf-8_mdbg.txt"
      B.readFile path

-- ccDict' :: CCDict
-- ccDict' = decode (BL.fromStrict raw)
--   where
--     raw = $(embedFile "data/cedict_1_0_ts_utf-8_mdbg.txt.binary")
