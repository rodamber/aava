module Haskell where

import Data.Maybe
import qualified Data.Text as T

--------------------------------------------------------------------------------

type Txt = T.Text
type Pat = Txt

data Input = Input
  { inputText :: Txt
  , inputPattern :: Pat
  } deriving (Show, Eq)

data Output = Output
  { positions :: [Int]
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

reverseZAlgorithmSpec = zAlgorithmSpec . T.reverse

--------------------------------------------------------------------------------
