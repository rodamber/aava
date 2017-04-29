{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Lib where

import Data.Foldable
import qualified Data.Sequence as S
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

newVCForeignPtr :: T.Text -> IO (ForeignPtr (Vector CChar))
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

zAlgorithm' :: (Ptr (Vector CChar) -> IO (Ptr (Vector CInt)))
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

withText :: T.Text -> (Ptr (Vector CChar) -> IO a)-> IO a
withText t f = do
  txt_fp <- newVCForeignPtr t
  withForeignPtr txt_fp f

class FromVectorOf a where
  getSize :: Ptr (Vector a) -> Int
  getArray :: Ptr (Vector a) -> Ptr a

instance FromVectorOf CChar where
  getSize = fromIntegral . C.get_size_char
  getArray = C.get_array_char

instance FromVectorOf CInt where
  getSize = fromIntegral . C.get_size_int
  getArray = C.get_array_int

class Storable a => ToVectorOf a where
  fromArray :: Ptr a -> CInt -> IO (Ptr (Vector a))

  toVector :: [a] -> IO (Ptr (Vector a))
  toVector xs = do
    withArray xs $ \ptr -> do
      fromArray ptr (fromIntegral $ length xs)

instance ToVectorOf CInt where
  fromArray = C.from_array_int

instance ToVectorOf CChar where
  fromArray = C.from_array_char

withVectorPtr :: (FromVectorOf a, Storable a)
              => Ptr (Vector a)
              -> ([a] -> IO b)
              -> IO b
withVectorPtr v_ptr f = do
  v_fp <- newForeignPtr finalizerFree v_ptr
  withForeignPtr v_fp $ \v_ptr -> do
      f =<< peekArray (getSize v_ptr) (getArray v_ptr)

build :: (Ptr (Vector CChar) -> Ptr (Vector CInt) -> IO (Ptr (Vector CInt)))
      -> T.Text
      -> S.Seq Int
      -> S.Seq Int
build cbuild t bigN =
  unsafePerformIO $ do
    withText t $ \t_ptr -> do
      withList (fromIntegral <$> toList bigN) $ \bigN_ptr -> do
        bigL'_ptr <- cbuild t_ptr bigN_ptr
        withVectorPtr bigL'_ptr (return . fmap fromIntegral . S.fromList)
  where
    withList :: [CInt] -> (Ptr (Vector CInt) -> IO b) -> IO b
    withList xs f = do
      v_fp <- newForeignPtr C.finalizerVectorInt =<< toVector xs
      withForeignPtr v_fp f

buildBigL' = build C.build_big_l'
buildSmallL' = build C.build_small_l'


--------------------------------------------------------------------------------
