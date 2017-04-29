{-# LANGUAGE Strict       #-}
{-# LANGUAGE ViewPatterns #-}
module Haskell where

import Debug.Trace

import Control.Monad.State
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

s `index1` i = s `S.index` (i - 1)
update1 i = S.update (i - 1)

buildBigL' :: T.Text -> S.Seq Int
buildBigL' t = execState (traverse_ loop [1 .. n - 1]) (S.replicate n 0)
  where
    n = T.length t
    bigN = S.fromList $ reverseZAlgorithmSpec t

    loop :: Int -> State (S.Seq Int) ()
    loop j = do
      bigL' <- get
      let i = n - bigN `index1` j + 1
      put (update1 i j bigL')

filterWithIndex :: (Int -> a -> Bool) -> S.Seq a -> S.Seq a
filterWithIndex p s = snd <$> S.filter (uncurry p) indexedSeq
  where indexedSeq = S.zip (S.fromList [0 .. S.length s]) s

maximumMay :: (Ord a, Foldable t) => t a -> Maybe a
maximumMay t | null t    = Nothing
             | otherwise = Just (maximum t)

maximumDef :: (Ord a, Foldable t) => a -> t a -> a
maximumDef def = fromMaybe def . maximumMay

buildL' :: T.Text -> S.Seq Int
buildL' t =
  let n = T.length t
      bigN = S.fromList $ reverseZAlgorithmSpec t
      js   = filterWithIndex (\ix x -> (ix + 1) == x) bigN
  in (flip fmap) (S.fromList [1..n]) $ \i ->
       maximumDef 0 $ S.filter (<= n - i + 1) js

strongGoodSuffixPreprocessingSpec :: T.Text -> (S.Seq Int, S.Seq Int)
strongGoodSuffixPreprocessingSpec t = (buildBigL' t, buildL' t)

data Match = Match | Mismatch Int

strongGoodSuffixShiftSpec :: (S.Seq Int, S.Seq Int) -> Match -> Int
strongGoodSuffixShiftSpec (_, l') Match =
  S.length l' - l' `index1` 2
strongGoodSuffixShiftSpec (bigL', l') (Mismatch j)
  | bigL'i >  0 = n - bigL'i
  | bigL'i == 0 = n - l' `index1` i
  | i == 1      = 1
  where bigL'i = bigL' `index1` i
        n = S.length l'
        i = j + 1

--------------------------------------------------------------------------------
