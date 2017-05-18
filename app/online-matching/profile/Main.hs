{-# LANGUAGE GADTs #-}

module Main where

import Control.Monad
import Data.Traversable
import System.Environment
import System.Process
import Test.QuickCheck
import Text.Printf
import Text.Read

--------------------------------------------------------------------------------

data Algorithm = N | K | B deriving (Eq, Show)

run = do
  forM (map (2^) [5..30]) $ \txtSize -> do
    forM (map (2^) [3..7]) $ \patSize -> do
      when (txtSize >= patSize) $ do
        txt <- generate (textGen txtSize)
        pat <- generate (textGen patSize)

        (naiveTime, naiveMem, naiveCmps) <- profile N txt pat
        (kmpTime,   kmpMem,   kmpCmps)   <- profile K txt pat
        (bmTime,    bmMem,    bmCmps)    <- profile B txt pat

        let format = unwords (replicate 10 "%d") ++ "\n"
        printf format txtSize    patSize
                      naiveTime  naiveMem
                      kmpTime    kmpMem    kmpCmps
                      bmTime     bmMem     bmCmps

textGen :: Int -> Gen String
textGen n = resize n $ listOf1 $ elements ['A', 'C', 'T', 'G']

profile :: Algorithm -> String -> String -> IO (Int,Int,Int)
profile search txt pat = return (0,0,0)

time :: Algorithm -> String -> String -> IO (Int,Int)
time alg txt pat = do
  let cmd = "time.sh"
  let args = ["./cbits/online-matching/om"]

  (elapsed:rest) <- lines <$> readProcess cmd args (input alg txt pat)

  return $ case alg of
             N -> (read elapsed, 0)
             K -> (read elapsed, read $ head rest)
             B -> (read elapsed, read $ head rest)


main = do
  time N "AAAAAAAAT" "AAT"
  -- print =<< time N "AAAAAAAAT" "AAT"
  -- print =<< time K "AAAAAAAAT" "AAT"
  -- print =<< time B "AAAAAAAAT" "AAT"

input :: Algorithm -> String -> String -> String
input alg txt pat = unlines ["T " ++ txt, show alg ++ " " ++ pat, "X"]



--------------------------------------------------------------------------------
