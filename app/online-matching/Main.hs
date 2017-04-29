module Main where

import Data.Foldable

import qualified Data.Text as T
import System.Environment

import Lib
import qualified FFI as C
import qualified Haskell as HS
import Haskell (Search, Txt, Pat, Input(..), Output(..))

search :: [Search] -> Txt -> Pat -> IO ()
search ss txt pat = print $ fmap (\s -> s (Input txt pat)) ss

extra = do
  let t = "CC"

  putStr "Haskell:\n  "
  print $ HS.reverseZAlgorithmSpec t
  putStr "  "
  print $ HS.strongGoodSuffixPreprocessingSpec t

  putStr "C:\n  "
  print $ reverseZAlgorithm t
  putStr "  "
  print $ strongGoodSuffixPreprocessing t

  return ()

main = do
  extra
  C.main_
