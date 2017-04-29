{-# LANGUAGE Strict #-}
module Haskell where

import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Sequence as S
import qualified Data.Text as T

--------------------------------------------------------------------------------

type Txt = T.Text
type Pat = Txt

data Input = Input
  { inputText    :: Txt
  , inputPattern :: Pat
  } deriving (Show, Eq)

data Output = Output
  { positions   :: [Int]
  , comparisons :: Int
  } deriving (Eq, Show)

type Search = Input -> Output

--------------------------------------------------------------------------------

indexedTails :: T.Text -> [(Int, T.Text)]
indexedTails = init . zip [0..] . T.tails

matchIndex :: T.Text -> (Int, T.Text) -> Maybe Int
matchIndex "" _ = Nothing
matchIndex _ (_,"") = Nothing
matchIndex x (ix, y) = if x `T.isPrefixOf` y then Just ix else Nothing

-- in the naive algorithm we don't care about the number of comparisons
naive :: Search
naive (Input txt pat) = Output positions 0
  where positions = mapMaybe (matchIndex pat) (indexedTails txt)

--------------------------------------------------------------------------------

zMatchCount :: T.Text -> Int -> Int -> Int
zMatchCount t a b = length $ takeWhile (uncurry (==)) $ setup
  where setup = T.zip (T.drop (a - 1) t) (T.drop (b - 1) t)

zAlgorithmSpec :: T.Text -> [Int]
zAlgorithmSpec txt = 0 : (zMatchCount txt 1 <$> [2 .. T.length txt])

reverseZAlgorithmSpec = reverse . zAlgorithmSpec . T.reverse

--------------------------------------------------------------------------------

badCharPreprocessingSpec :: T.Text -> M.Map Char Int
badCharPreprocessingSpec t =
  (flip foldMap) ['A','C','T','G'] $ \c ->
    case T.findIndex (== c) (T.reverse t) of
      Just ix -> M.singleton c (len - 1 - ix)
      Nothing -> M.empty
  where len = T.length t

badCharShiftSpec :: M.Map Char Int -> Int -> Char -> Int
badCharShiftSpec r ix c = max 1 (ix - x)
  where x = case M.lookup c r of
              Just a -> a
              Nothing -> error "bad char shift: impossible char"

--------------------------------------------------------------------------------

buildBigL' :: T.Text -> S.Seq Int
buildBigL' t = S.fromList $
  (flip fmap) [1 .. n] $ \i -> maximum $
    (flip fmap) [i .. n - 1] $ \j ->
      if bigN `S.index` j == n - i + 1 then j else 0
  where
    n = T.length t
    bigN = S.fromList (reverseZAlgorithmSpec t)

buildL' :: T.Text -> S.Seq Int
buildL' t = S.fromList $
  (flip fmap) [1 .. n] $ \i -> maximum $
    (flip fmap) [1 .. n - i + 1] $ \j ->
      if bigN `S.index` j == j then j else 0
  where
    n = T.length t
    bigN = S.fromList (reverseZAlgorithmSpec t)

strongGoodSuffixPreprocessingSpec :: T.Text -> (S.Seq Int, S.Seq Int)
strongGoodSuffixPreprocessingSpec t = (buildBigL' t, buildL' t)

data Match = Match | Mismatch Int

strongGoodSuffixShiftSpec :: (S.Seq Int, S.Seq Int) -> Match -> Int
strongGoodSuffixShiftSpec (_, l') Match =
  S.length l' - l' `S.index` 2
strongGoodSuffixShiftSpec (bigL', l') (Mismatch j)
  | bigL'i >  0 = n - bigL'i
  | bigL'i == 0 = n - l' `S.index` i
  | i == 1      = 1
  where bigL'i = bigL' `S.index` i
        n = S.length l'
        i = j + 1

--------------------------------------------------------------------------------
