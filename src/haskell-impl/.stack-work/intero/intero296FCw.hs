{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeSynonymInstances     #-}

module Lib where

import qualified Data.Text as T

import Foreign
import Foreign.C.Types
import Foreign.C.String

import System.IO.Unsafe (unsafePerformIO)

import qualified C
import C (Result, Vector)

import qualified Haskell as HS
import Haskell (Txt, Pat, Input(..), Output(..))

--------------------------------------------------------------------------------

addPosition :: Output -> Int -> Output
addPosition (Output ps c) p = Output (p:ps) c
  -- Note that here we're preprending instead of appending

count :: Output -> Output
count (Output ps c) = Output ps (c + 1)

mkOutput :: (Ptr Result) -> Output
mkOutput res_ptr =
  let n = fromIntegral $ C.get_positions_size res_ptr
      positions = unsafePerformIO $ do
        pos_ptr <- newForeignPtr finalizerFree =<< C.get_positions res_ptr
        withForeignPtr pos_ptr (peekArray n)
      comparisons = C.get_comparisons res_ptr
  in Output (fromIntegral <$> positions) (fromIntegral comparisons)

runSearch :: C.Search -> HS.Search
runSearch search (Input txt pat) = unsafePerformIO $ do
  txt_fp <- newForeignPtr C.finalizerVectorChar =<< newVectorChar txt
  pat_fp <- newForeignPtr C.finalizerVectorChar =<< newVectorChar pat

  withForeignPtr txt_fp $ \txt' -> do
    withForeignPtr pat_fp $ \pat' -> do
      res_fp <- newForeignPtr C.finalizerResult =<< search txt' pat'
      withForeignPtr res_fp (return . mkOutput)
  where
    newVectorChar t = C.new_string =<< newCString (T.unpack t)

--------------------------------------------------------------------------------

class StringMatch a where
  match :: a -> HS.Search

instance StringMatch HS.Search where
  match = id

instance StringMatch C.Search where
  match = runSearch

--------------------------------------------------------------------------------

