{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Foldable

import qualified Data.Text as T
import System.Environment

import qualified C
import C (Result, Vector)

import qualified Haskell as HS
import Haskell (Txt, Pat, Input(..), Output(..))

import Lib

search :: StringMatch s => [s] -> Txt -> Pat -> IO ()
search ss txt pat = print $ fmap (\s -> match s (Input txt pat)) ss

main :: IO ()
main = do
  [txt, pat] <- (fmap T.pack) <$> getArgs
  search [HS.naive] txt pat
  search [C.naive, C.knuth_morris_pratt, C.boyer_moore] txt pat
