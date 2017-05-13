{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------------------

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Data.Foldable
import Data.Traversable
import Data.List
import Data.Maybe

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Monadic
import qualified Test.QuickCheck.Property as P

import System.Process

--------------------------------------------------------------------------------

data Node
type NodePtr = Ptr Node

foreign import ccall unsafe "alloc" c_alloc :: CInt -> IO NodePtr
foreign import ccall unsafe "link" c_link :: CInt -> CInt -> IO ()
foreign import ccall unsafe "cut" c_cut :: CInt -> CInt -> IO ()
foreign import ccall unsafe "connected" c_connected :: CInt -> CInt -> IO CInt
foreign import ccall unsafe "pnode2" c_pnode2 :: CInt -> IO ()
foreign import ccall unsafe "pstate" pstate :: IO ()

alloc = c_alloc . fromIntegral

link      x y = putStrLn s >> c_link      (fromIntegral x) (fromIntegral y) >> pstate
  where s = "=== L " ++ show x ++ " " ++ show y
cut       x y = putStrLn s >> c_cut       (fromIntegral x) (fromIntegral y) >> pstate
  where s = "=== C " ++ show x ++ " " ++ show y
connected x y = putStrLn s >> c_connected (fromIntegral x) (fromIntegral y) >> pstate
  where s = "=== Q " ++ show x ++ " " ++ show y

pnode :: Int -> IO ()
pnode x = putStr "  " >> pnode (fromIntegral x)

withForest :: Int -> IO a -> IO a
withForest forestSize io = do
  ptr <- alloc forestSize
  ret <- io
  free ptr
  return ret

--------------------------------------------------------------------------------

data Cmd = L Int Int
         | C Int Int
         | Q Int Int
         | X
         deriving (Show, Eq)

data TestCase = TestCase { forestSize :: Int
                         , commands :: [Cmd]
                         } deriving (Eq)

instance Show TestCase where
  show (TestCase n cmds) = show n ++ "\n" ++ unlines (map show cmds)

cmdGen :: Int -> Gen Cmd
cmdGen n = do
  x <- choose (1, n)
  y <- choose (1, n) `suchThat` (/= x)
  cmd <- frequency [(6, pure L),(2, pure C),(1, pure Q)]
  return (cmd x y)

instance Arbitrary TestCase where
  arbitrary = do
    n <- (getPositive <$> arbitrary) `suchThat` (\x -> 5 < x && x <= 30)
    -- let n = 4
    cmds <- resize 30 $ listOf1 $ cmdGen n
    return (TestCase { forestSize = n , commands = reverse (X : cmds)})

--------------------------------------------------------------------------------

testCase :: TestCase -> String -> IO String
testCase tc exec = do
  let cmd = "cbits/link-cut/" ++ exec
  readProcess cmd [] (show tc)

prop_meetsSpec tc = monadicIO $ do
  expected <- run $ testCase tc "lc_spec"
  actual <- run $ testCase tc "lc"
  assert (actual == expected)

main = do
  putStrLn ""

  quickCheckWith stdArgs { maxSuccess = 10000 } prop_meetsSpec

  -- withForest 4 $ do
  --   link 3 2
  --   link 3 4
  --   cut  4 2
  --   print =<< connected 2 3

{-
=== Input === X
=== Actual/Expected===
-}
