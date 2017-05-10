{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

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

import Data.DeriveTH

--------------------------------------------------------------------------------

data Node
type NodePtr = Ptr Node

foreign import ccall unsafe "alloc" c_alloc :: CInt -> IO NodePtr
foreign import ccall unsafe "link_" c_link :: CInt -> CInt -> IO ()
foreign import ccall unsafe "cut_" c_cut :: CInt -> CInt -> IO ()
foreign import ccall unsafe "connected_" c_connected :: CInt -> CInt -> IO Bool

alloc = c_alloc . fromIntegral
link x y = c_link (fromIntegral x) (fromIntegral y)
cut x y = c_cut (fromIntegral x) (fromIntegral y)
connected x y = c_connected (fromIntegral x) (fromIntegral y)

withForest :: (Positive Int) -> IO a -> IO a
withForest (Positive forestSize) io =
  do ptr <- alloc forestSize
     ret <- io
     free ptr
     return ret

--------------------------------------------------------------------------------

data Command a = Link      (Positive a) (Positive a)
               | Cut       (Positive a) (Positive a)
               | Connected (Positive a) (Positive a)
               deriving (Show, Eq)

$(derive makeIs ''Command)

type Cmd = Command Int

inputs :: Cmd -> (Int,Int)
inputs (Link      (Positive x) (Positive y)) = (x,y)
inputs (Cut       (Positive x) (Positive y)) = (x,y)
inputs (Connected (Positive x) (Positive y)) = (x,y)

genCmd :: Positive Int -> Gen Cmd
genCmd (Positive n) = do
  cmd <- elements [Link, Cut, Connected]
  x <- choose (1, n - 1)
  y <- choose (x, n)
  return (cmd (Positive x) (Positive y))

runCommand :: Cmd -> IO ()
runCommand (Link      (Positive x) (Positive y)) = link x y
runCommand (Cut       (Positive x) (Positive y)) = cut x y
runCommand (Connected (Positive x) (Positive y)) = connected x y

data Input = Input { forestSize :: (Positive Int)
                   , commands   :: [Cmd]
                   } deriving (Show, Eq)

instance Arbitrary Input where
  arbitrary = do
    forestSize <- arbitrary `suchThat` ((> 3) . getPositive)
    cmds <- listOf1 (genCmd forestSize)
    return (Input forestSize cmds)

inputSuchThat :: (Input -> Bool) -> Gen Input
inputSuchThat p = arbitrary `suchThat` p

linkThenCut :: Gen Input
linkThenCut = inputSuchThat $ \(Input _ cmds) ->
  let fst = head cmds
      snd = head (tail cmds)
  in length cmds == 2 &&
     isLink fst &&
     isCut  snd &&
     inputs fst == inputs snd

--------------------------------------------------------------------------------

prop_linkThenCutIsNotConnected (Input forestSize commands) =
  monadicIO $ do
    c <- run $ withForest forestSize $ do
      traverse runCommand commands
      return (uncurry connected (inputs (head commands)))
    assert (not c)

-- return []
main = quickCheck (forAll linkThenCut prop_linkThenCutIsNotConnected)
