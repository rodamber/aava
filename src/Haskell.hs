{-# LANGUAGE Strict       #-}
{-# LANGUAGE ViewPatterns #-}
module Haskell where

import Debug.Trace

import Control.Monad.State.Strict
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
      Just ix -> M.singleton c (T.length t - ix)
      Nothing -> M.singleton c 0

badCharShiftSpec :: M.Map Char Int -> Int -> Char -> Int
badCharShiftSpec r ix c = max 1 (ix - x)
  where x = case M.lookup c r of
              Just a -> a
              Nothing -> error "bad char shift: impossible char"

--------------------------------------------------------------------------------

s `index1` i = s `S.index` (i - 1)
update1 i = S.update (i - 1)

buildBigL'Spec :: S.Seq Int -> S.Seq Int
buildBigL'Spec bigN = execState (traverse_ loop [1 .. n - 1]) (S.replicate n 0)
  where
    n = length bigN

    loop :: Int -> State (S.Seq Int) ()
    loop j = modify' (update1 i j )
      where i = n - bigN `index1` j + 1

filterWithIndex :: (Int -> a -> Bool) -> S.Seq a -> S.Seq a
filterWithIndex p s = snd <$> S.filter (uncurry p) indexedSeq
  where indexedSeq = S.zip (S.fromList [0 .. length s]) s

maximumMay :: (Ord a, Foldable t) => t a -> Maybe a
maximumMay t | null t    = Nothing
             | otherwise = Just (maximum t)

maximumDef :: (Ord a, Foldable t) => a -> t a -> a
maximumDef def = fromMaybe def . maximumMay

buildSmallL'Spec :: S.Seq Int -> S.Seq Int
buildSmallL'Spec bigN =
  let n  = length bigN
      js = filterWithIndex (\ix x -> (ix + 1) == x) bigN
  in (flip fmap) (S.fromList [1..n]) $ \i ->
       maximumDef 0 $ S.filter (<= n - i + 1) js

data Match = Match | Mismatch Int
  deriving (Eq, Show)

strongGoodSuffixShiftSpec :: (S.Seq Int, S.Seq Int) -> Match -> Int
strongGoodSuffixShiftSpec (length -> 1, _) _ = 1
strongGoodSuffixShiftSpec (_, l') Match = length l' - index1 l' 2
strongGoodSuffixShiftSpec (bigL', l') (Mismatch ix)
  | ix == n     = 1
  | bigL'i >  0 = n - bigL'i
  | bigL'i == 0 = n - l'i
  where
    n = length l'
    i = ix + 1
    (bigL'i, l'i) = (index1 bigL' i, index1 l' i)
--------------------------------------------------------------------------------
