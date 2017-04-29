{-# LANGUAGE OverloadedStrings #-}
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

main = do
  C.main_
