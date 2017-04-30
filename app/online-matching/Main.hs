module Main where

import Data.Foldable

import qualified Data.Sequence as S
import qualified Data.Text as T
import System.Environment

import Lib
import qualified FFI as C
import qualified Haskell as HS
import Haskell (Search, Txt, Pat, Input(..), Output(..))

import Debug.Trace

search :: [Search] -> Txt -> Pat -> IO ()
search ss txt pat = print $ fmap (\s -> s (Input txt pat)) ss

extra = do
  print $ and $ (flip fmap) [1 .. ceiling 1e4] $ \i ->
    trace (show i) buildBigL' (S.fromList [-20,26,-2,29,-1,22]) == buildBigL' (S.fromList [-20,26,-2,29,-1,22])

main = do
  extra
  -- C.main_
