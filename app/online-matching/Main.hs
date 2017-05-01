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
  let t = "CGTGCGG"
  let r = badCharPreprocessing t
  let (i,c) = (7,'T')

  putStr "HS R: " >> print r

  print $ HS.badCharShiftSpec r i c
  print $ badCharShift r i c

main = do
  extra
  -- C.main_
