{-# LANGUAGE GADTs #-}

module Main where

import Control.Monad

import qualified Data.ByteString.Char8 as C
import Data.Traversable
import Data.Vector

import System.Environment
import System.IO
import System.Process
import System.Random

import Text.Printf
import Text.Read

chars :: Vector Char
chars = fromList ['A','C','T','G']

main :: IO ()
main = do
  forM_ (map (2^) [5..20]) $ \txtSize -> do
    forM_ (map (2^) [3..10]) $ \patSize -> do

      when (txtSize >= patSize) $ do
        let fileName = _ -- makeFileName txtSize patSize
        write fileName txtSize patSize

write :: String -> Int -> Int -> IO ()
write = undefined

        -- ix <- randomIO
        -- let ch = chars ! (ix `mod` 4)

        -- putStr "T "
        -- putChar ch

