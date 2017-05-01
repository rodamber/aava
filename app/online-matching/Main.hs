module Main where

import Data.Foldable

import qualified Data.Sequence as S
import qualified Data.Text as T
import System.Environment

import Lib
import qualified FFI as C
import qualified Haskell as HS
import Haskell (Search, Txt, Pat, Input(..), Output(..), Match(..))

import Debug.Trace

search :: [Search] -> Txt -> Pat -> IO ()
search ss txt pat = print $ fmap (\s -> s (Input txt pat)) ss

extra = do
  let t = "TCC"
  let m = Mismatch 3
  let n = S.fromList $ HS.reverseZAlgorithmSpec t
  let ls = (HS.buildBigL'Spec n, HS.buildSmallL'Spec n)

  print n
  print ls
  print (buildBigL' n, buildSmallL' n)

  print $ HS.strongGoodSuffixShiftSpec ls m
  print $ strongGoodSuffixShift ls m

main = do
  extra
  -- C.main_
