{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE TypeSynonymInstances     #-}

module Lib where

import qualified Data.Text as T

import Foreign
import Foreign.C.Types
import Foreign.C.String

import System.IO.Unsafe (unsafePerformIO)

import qualified FFI as C
import FFI (Result, Vector)

import qualified Haskell as HS
import Haskell (Txt, Pat, Input(..), Output(..))

--------------------------------------------------------------------------------

newVCForeignPtr :: T.Text -> IO (ForeignPtr (Vector Char))
newVCForeignPtr txt = newForeignPtr C.finalizerVectorChar =<< newVectorChar txt
  where newVectorChar t = C.new_string =<< newCString (T.unpack t)

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
        pos_fp <- newForeignPtr finalizerFree =<< C.get_positions res_ptr
        withForeignPtr pos_fp (peekArray n)
      comparisons = C.get_comparisons res_ptr
  in Output (fromIntegral <$> positions) (fromIntegral comparisons)

runSearch :: C.Search -> HS.Search
runSearch search (Input txt pat) =
  unsafePerformIO $ do
    txt_fp <- newVCForeignPtr txt
    pat_fp <- newVCForeignPtr pat

    withForeignPtr txt_fp $ \txt' -> do
      withForeignPtr pat_fp $ \pat' -> do
        res_fp <- newForeignPtr C.finalizerResult =<< search txt' pat'
        withForeignPtr res_fp (return . mkOutput)

--------------------------------------------------------------------------------

naive = runSearch C.naive
knuthMorrisPratt = runSearch C.knuth_morris_pratt
boyerMoore = runSearch C.boyer_moore

--------------------------------------------------------------------------------

zAlgorithm' :: (Ptr (Vector Char) -> IO (Ptr (Vector Int)))
           -> T.Text -> [Int]
zAlgorithm' algo txt =
  unsafePerformIO $ do
    txt_fp <- newVCForeignPtr txt

    withForeignPtr txt_fp $ \txt' -> do
      z_fp <- newForeignPtr C.finalizerVectorInt =<< algo txt'

      withForeignPtr z_fp $ \z_ptr -> fmap fromIntegral <$>
        peekArray (fromIntegral $ C.get_size_int z_ptr) (C.get_array_int z_ptr)

zAlgorithm = zAlgorithm' C.z_algorithm
reverseZAlgorithm = zAlgorithm' C.reverse_z_algorithm

--------------------------------------------------------------------------------

reverseChar :: T.Text -> T.Text
reverseChar txt =
  unsafePerformIO $ do
    txt_fp <- newVCForeignPtr txt

    withForeignPtr txt_fp $ \txt' -> do
      rev_fp <- newForeignPtr C.finalizerVectorChar =<< C.reverse_char txt'

      withForeignPtr rev_fp $ \rev_ptr -> T.pack . fmap castCCharToChar <$>
        peekArray (fromIntegral $ C.get_size_char rev_ptr) (C.get_array_char rev_ptr)

--------------------------------------------------------------------------------
