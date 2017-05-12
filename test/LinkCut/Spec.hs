{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Control.Monad
import Data.Foldable
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Text as T

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Monadic

--------------------------------------------------------------------------------

data Node
type NodePtr = Ptr Node

foreign import ccall unsafe "alloc" c_alloc :: CInt -> IO NodePtr
foreign import ccall unsafe "link_" c_link :: CInt -> CInt -> IO ()
foreign import ccall unsafe "cut_" c_cut :: CInt -> CInt -> IO ()
foreign import ccall unsafe "connected_" c_connected :: CInt -> CInt -> IO Bool
foreign import ccall unsafe "pnode2" c_pnode2 :: CInt -> IO ()

alloc = c_alloc . fromIntegral . getPositive

link      x y = c_link      (fromIntegral (getPositive x))
                            (fromIntegral (getPositive y))
cut       x y = c_cut       (fromIntegral (getPositive x))
                            (fromIntegral (getPositive y))
connected x y = c_connected (fromIntegral (getPositive x))
                            (fromIntegral (getPositive y))

pnode :: Int -> IO ()
pnode = c_pnode2 . fromIntegral

withForest :: (Positive Int) -> IO a -> IO a
withForest forestSize  io =
  do ptr <- alloc forestSize
     ret <- io
     free ptr
     return ret

--------------------------------------------------------------------------------

data Link
data Cut
data Connected

class CmdType ct input output | ct -> input, ct -> output where
  data Cmd ct :: *

  mkCmd :: input -> Cmd ct
  inputs :: Cmd ct -> input
  runCmd :: Cmd ct -> IO output

instance CmdType Link (Positive Int, Positive Int) () where
  data Cmd Link = CmdLink (Positive Int) (Positive Int)
    deriving (Eq, Show)

  mkCmd (x,y) = CmdLink x y
  inputs (CmdLink x y) = (x,y)
  runCmd (CmdLink x y) = link x y

instance CmdType Cut (Positive Int, Positive Int) () where
  data Cmd Cut = CmdCut (Positive Int) (Positive Int)
    deriving (Eq, Show)

  mkCmd (x,y) = CmdCut x y
  inputs (CmdCut x y) = (x,y)
  runCmd (CmdCut x y) = cut x y

instance CmdType Connected (Positive Int, Positive Int) Bool where
  data Cmd Connected = CmdConnected (Positive Int) (Positive Int)
    deriving (Eq, Show)

  mkCmd (x,y) = CmdConnected x y
  inputs (CmdConnected x y) = (x,y)
  runCmd (CmdConnected x y) = connected x y

genCmd :: CmdType ct (Positive Int, Positive Int) o
       => Positive Int -> Gen (Cmd ct)
genCmd (Positive n) =
  do x <- choose (1, n - 1)
     y <- choose (x, n)
     return (mkCmd (Positive x, Positive y))

--------------------------------------------------------------------------------

genForestSize :: Gen (Positive Int)
genForestSize = arbitrary `suchThat` ((> 10) . getPositive)

genInput :: Positive Int -> Gen (Positive Int, Positive Int)
genInput (Positive n) =
  do x <- choose (1, n - 1)
     y <- choose (x, n)
     return (Positive x, Positive y)

genProblem =
  do n <- genForestSize
     (x,y) <- genInput n
     return (n, x, y)

--------------------------------------------------------------------------------

genLinkThenCut :: Gen (Positive Int, Cmd Link, Cmd Cut, Cmd Connected)
genLinkThenCut =
  do forestSize <- genForestSize
     input <- genInput forestSize
     return (forestSize, mkCmd input, mkCmd input, mkCmd input)

prop_linkThenCutIsNotConnected (forestSize, link, cut, connected) =
  monadicIO $ do
    conn <- run $ withForest forestSize $ do
      runCmd link
      runCmd cut
      runCmd connected
    assert (not conn)

prop_linkThenCutIsNotConnected2 (forestSize, x, y) =
  monadicIO $ do
    conn <- run $ withForest forestSize $ do
      link x y
      cut x y
      connected x y
    assert (not conn)

-- --------------------------------------------------------------------------------

-- -- return []
-- main = verboseCheck (forAll genLinkThenCut prop_linkThenCutIsNotConnected)
-- main = verboseCheck (forAll genProblem prop_linkThenCutIsNotConnected2)
-- main = putStrLn "test suite not implemented"

instance Num a => Num (Positive a) where
  (Positive x) + (Positive y) = Positive (x + y)
  (Positive x) * (Positive y) = Positive (x * y)
  abs = fmap abs
  signum = fmap signum
  fromInteger = Positive . fromInteger
  negate = fmap negate

printNodes n = traverse_ pnode [1 .. getPositive n] >> putStrLn ""

main = do
  let n = Positive 5
  withForest n $ do
    printNodes n
    link 1 2
    link 2 3
    link 3 4
    quickCheck (prop_linkThenCutIsNotConnected2 (n, 1, 2))
    printNodes n
