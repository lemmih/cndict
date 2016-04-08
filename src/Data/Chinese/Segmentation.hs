{-# LANGUAGE OverloadedStrings #-}
module Data.Chinese.Segmentation
  ( Token(..)
  , Entry(..)
  , entrySimplified
  , entryTraditional
  , entryPinyin

  , tokenizer
  , ppTokens
  , toTraditional
  , toSimplified
  ) where

import Data.Chinese.CCDict (Entry(..), entrySimplified, entryTraditional, entryWordFrequency, entryPinyin)
import qualified Data.Chinese.CCDict as CC
import qualified Data.Text as T
import Data.Text (Text)
import Data.List
import Data.Maybe
import Control.Monad
import Data.Ord

data Token = KnownWord Entry | UnknownWord Text
  deriving ( Read, Show, Eq, Ord )

--
-- ABC
-- [[A,AB],[B],[C]]
-- [ [[A,B],[AB]]
-- , [[C]] ]
splitText :: Text -> [[Token]]
splitText txt =
  [ case CC.lookupMatches offset of
      Nothing -> [UnknownWord char]
      Just entries -> map KnownWord entries
  | offset <- T.tails txt
  , not (T.null offset)
  , let char = T.take 1 offset ]

-- [[A,AB],[B]] -> [ [[A,B],[AB]] ]
-- [[A,AB],[BC],[C]] -> [[[A,BC],[AB,C]]]
-- [[A,AB],[B,BC],[C]] -> [[[A,B,C],[A,BC],[AB,C]]]
-- [[A,ABC],[B],[C]]
findGroups :: [[Token]] -> [[[Token]]]
findGroups [] = []
findGroups tokens =
    nub (map trim (sequence lst)) : findGroups rest
  where
    lst = take len tokens
    rest = drop len tokens
    len = groupLength tokens
    trim [] = []
    trim (t:ts) = t : trim (drop (tokenLength t-1) ts)

groupLength :: [[Token]] -> Int
groupLength = worker 1
  where
    worker l [] = 0
    worker 0 _  = 0
    worker l (ts:tss) =
      let maxLength = maximum (map tokenLength ts) in
      1 + worker (max l maxLength - 1) tss

filterExceptions :: [[Token]] -> [[Token]]
filterExceptions lst = worker lst
  where
    worker (x:xs) =
      let ws = [ entrySimplified e | KnownWord e <- x ] in
      case ws `elem` exceptions of
        False -> worker xs
        True  -> [x]
    worker [] = lst
    exceptions =
      [["家", "中餐馆"]
      ,["这", "位子"]
      ,["十", "分钟"]
      ,["一", "点钟"]
      ,["合上", "书"]]

greedyGroups :: [[[Token]]] -> [[[Token]]]
greedyGroups = map worker
  where
    worker tss =
      filter (onlyWithLength (minimum $ map length tss)
                             (maximum $ map (maximum.map tokenLength) tss)) tss
    onlyWithLength len tlen ts =
      length ts == len -- && maximum (map tokenLength ts) == tlen

tokenLength UnknownWord{} = 0
tokenLength (KnownWord e) = T.length (entrySimplified e)

flattenGroups :: [[[Token]]] -> [Token]
flattenGroups = joinUnknownWords . concatMap pickBest
  where
    joinUnknownWords [] = []
    joinUnknownWords (UnknownWord a:UnknownWord b:rest) =
      joinUnknownWords (UnknownWord (a `T.append` b):rest)
    joinUnknownWords (x:xs) =
      x : joinUnknownWords xs

pickBest :: [[Token]] -> [Token]
pickBest lst =
    snd (maximumBy (comparing fst) graded)
  where
    graded = [ (score x,x) | x <- lst ]
    score x = geoMean (mapMaybe tokenScore x)

ppGroups :: [[[Token]]] -> String
ppGroups = unwords . map worker
  where
    worker :: [[Token]] -> String
    worker [] = []
    worker tss = "{" ++ unwords (intersperse "|" $ map ppGroup tss) ++ "}"
    ppGroup :: [Token] -> String
    ppGroup ts = unwords (map ppToken ts)
    ppToken (UnknownWord txt) = T.unpack txt
    ppToken (KnownWord e) = T.unpack (entrySimplified e)

ppTokens :: [Token] -> String
ppTokens = unwords . map toString
  where
    toString (UnknownWord txt) = T.unpack txt
    toString (KnownWord e) = T.unpack (entrySimplified e)

geoMean :: [Int] -> Int
geoMean ls = round $
  fromIntegral (product ls) ** recip (fromIntegral (length ls))

tokenScore :: Token -> Maybe Int
tokenScore UnknownWord{} = Nothing
tokenScore (KnownWord e)
  -- | T.length (entrySimplified e) == 1 = Nothing
  | otherwise = Just $ entryWordFrequency e

--wordCount :: Text -> Int
--wordCount txt =
--  case F.wordFrequency txt of
--    Just n -> n
--    Nothing -> 0 {-minimum
--      [ M.findWithDefault 1 char F.freqMap
--      | char <- T.chunksOf 1 txt ]-}

-- | Break a string of simplified chinese down to a list of tokens.
tokenizer :: Text -> [Token]
tokenizer =
  flattenGroups . greedyGroups . map filterExceptions .
  findGroups . splitText

_ppSegmentationTests =
    forM_ wrong $ \(txt, expected, got) -> do
      putStrLn $ "Fail: " ++ T.unpack txt ++
           ", expected: " ++ expected ++
           ", got: " ++ got
  where
    wrong =
      [ (txt, expected, got)
      | (txt, expected) <- cases
      , let got = ppTokens $ tokenizer txt
      , got /= expected]
    cases =
        [ ("多工作", "多 工作")
        , ("有电话", "有 电话")
        , ("回电话", "回 电话")
        , ("不知道", "不 知道")
        , ("定时间", "定 时间")
        , ("这位子", "这 位子")
        , ("十分钟", "十 分钟")
        , ("有电梯", "有 电梯")
        , ("中午前", "中午 前")
        , ("好心地", "好心 地")
        , ("想要点", "想要 点")
        , ("得很", "得 很")
        -- , ("不想", ["不","想"])
        -- , ("那是", ["那","是"])
        , ("外套", "外套")
        , ("家中餐馆", "家 中餐馆")
        , ("后生活", "后 生活")
        , ("不愿意", "不 愿意")
        , ("点出发", "点 出发")
        , ("老婆婆", "老 婆婆")
        , ("不会跳舞", "不会 跳舞")
        , ("穿上外套", "穿上 外套")
        , ("建议", "建议")
        , ("怎么不知道", "怎么 不 知道")
        , ("蛋糕发起来", "蛋糕 发 起来")
        , ("管理的人才", "管理 的 人才")
        , ("轻快乐曲", "轻快 乐曲")
        , ("高明和", "高明 和")
        , ("一下子之间", "一下子 之间")
        , ("我绝没想到", "我 绝 没想到")
        , ("没想到会", "没想到 会")

        , ("公园里人挤人","公园 里 人 挤 人")
        , ("我可没有时间闲呆着","我 可 没有 时间 闲 呆 着")
        , ("你定时间吧","你 定 时间 吧")
        -- , ("这位子有人吗","这 位子 有人 吗")
        , ("我要看病","我 要 看病")
        , ("你好像不太舒服","你 好像 不 太 舒服")
        , ("我非常想见到她","我 非常 想 见到 她")
        , ("能认识你我非常幸福","能 认识 你 我 非常 幸福")
        , ("没有你我无法活下去","没有 你 我 无法 活下去")
        , ("为你我在所不惜","为 你 我 在 所 不惜")
        , ("婚后生活怎么样","婚 后 生活 怎么样")
        , ("我是个顾家的人","我 是 个 顾 家 的 人")
        , ("我有好多事要干","我 有 好多 事 要 干")
        , ("我不知道这张表怎么填","我 不 知道 这 张 表 怎么 填")
        , ("我有很多事要做","我 有 很 多 事 要 做")
        , ("我不知道他在想什么","我 不 知道 他 在 想 什么")
        , ("我是个不顾家的人","我 是 个 不顾 家 的 人")
        , ("你真有胆量","你 真 有胆量")
        , ("夏天到了", "夏天 到 了")
        , ("我先做作业再吃晚饭","我 先 做 作业 再 吃 晚饭")
        , ("现在一点钟了", "现在 一 点钟 了")

        , ("我合上书准备离开", "我 合上 书 准备 离开")
        , ("他的话","他 的 话")
        , ("你用什么方法学习","你 用 什么 方法 学习")
        ]












--------------------------------------------------
-- Simplified <-> Traditional

flatMap :: (Entry -> Text) -> [Token] -> Text
flatMap fn = T.concat . map worker
  where
    worker (KnownWord e)     = fn e
    worker (UnknownWord txt) = txt

toTraditional :: Text -> Text
toTraditional = flatMap entryTraditional . tokenizer

toSimplified :: Text -> Text
toSimplified = flatMap entrySimplified . tokenizer
