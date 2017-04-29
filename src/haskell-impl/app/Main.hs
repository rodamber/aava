{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Foldable

import qualified Data.Text as T
import System.Environment

import qualified C
import C (Result, Vector)

import qualified Haskell as HS
import Haskell (Search, Txt, Pat, Input(..), Output(..))

import Lib

search :: [Search] -> Txt -> Pat -> IO ()
search ss txt pat = print $ fmap (\s -> s (Input txt pat)) ss

-- main = do
--   [txt, pat] <- (fmap T.pack) <$> getArgs
--   search [HS.naive, naive, knuthMorrisPratt, boyerMoore] txt pat

main = do
  print $ zAlgorithm "aabcaabxaaz"
  print $ HS.zAlgorithmSpec "aabcaabxaaz"
