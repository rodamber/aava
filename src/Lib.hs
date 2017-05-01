{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib where

import Control.Monad
import Data.Foldable
import Data.Maybe
import qualified Data.Map as M
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
-- FFI General
--------------------------------------------------------------------------------

newVCForeignPtr :: T.Text -> IO (ForeignPtr (Vector CChar))
newVCForeignPtr txt = newForeignPtr C.finalizerVectorChar =<< newVectorChar txt
  where newVectorChar t = C.new_string =<< newCString (T.unpack t)

class Storable a => MkVector a where
  getSize   :: Ptr (Vector a) -> Int
  getArray  :: Ptr (Vector a) -> Ptr a
  fromArray :: Ptr a -> CInt -> IO (Ptr (Vector a))
  finalizer :: FunPtr (Ptr (Vector a) -> IO ())

instance MkVector CChar where
  getSize   = fromIntegral . C.get_size_char
  getArray  = C.get_array_char
  fromArray = C.from_array_char
  finalizer = C.finalizerVectorChar

instance MkVector CInt where
  getSize   = fromIntegral . C.get_size_int
  getArray  = C.get_array_int
  fromArray = C.from_array_int
  finalizer = C.finalizerVectorInt

fromVector :: MkVector a => Ptr (Vector a) -> IO [a]
fromVector ptr = do
  fp <- newForeignPtr finalizer ptr
  withForeignPtr fp $ \ptr' ->
    peekArray (getSize ptr') (getArray ptr')

toVector :: MkVector a => [a] -> IO (ForeignPtr (Vector a))
toVector xs =
  withArray xs $ \ptr -> newForeignPtr finalizer =<<
    fromArray ptr (fromIntegral $ length xs)

withText :: T.Text -> (Ptr (Vector CChar) -> IO a)-> IO a
withText t f = (flip withForeignPtr) f =<< newVCForeignPtr t

withList :: MkVector a => [a] -> (Ptr (Vector a) -> IO b) -> IO b
withList xs f = (flip withForeignPtr) f =<< toVector xs

withSeq :: MkVector a => S.Seq a -> (Ptr (Vector a) -> IO b) -> IO b
withSeq s = withList (toList s)

--------------------------------------------------------------------------------
-- Searches
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

naive = runSearch C.naive
knuthMorrisPratt = runSearch C.knuth_morris_pratt
boyerMoore = runSearch C.boyer_moore

--------------------------------------------------------------------------------
-- Z Algorithm
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

reverseChar :: T.Text -> T.Text
reverseChar txt =
  unsafePerformIO $ do
    txt_fp <- newVCForeignPtr txt

    withForeignPtr txt_fp $ \txt' -> do
      rev_fp <- newForeignPtr C.finalizerVectorChar =<< C.reverse_char txt'

      withForeignPtr rev_fp $ \rev_ptr -> T.pack . fmap castCCharToChar <$>
        peekArray (fromIntegral $ C.get_size_char rev_ptr) (C.get_array_char rev_ptr)

--------------------------------------------------------------------------------
-- Boyer Moore
--------------------------------------------------------------------------------

badCharPreprocessing :: T.Text -> M.Map Char Int
badCharPreprocessing t =
  unsafePerformIO $ do
    withText t $ \txt -> do
      ints <- fromVector =<< (flip fromArray) 4 =<< C.bad_char_preprocessing txt
      return $ M.fromList $ zip ['A','C','T','G'] (fromIntegral <$> ints)

badCharShift :: M.Map Char Int -> Int -> Char -> Int
badCharShift m ix c =
  unsafePerformIO $ do
    withMap m $ \ptr -> do
      cm <- C.new_match 0 (fromIntegral ix) 
      let res = C.bad_char_shift ptr cm (castCharToCChar c)
      free cm
      return $ fromIntegral res
  where
    withMap :: M.Map Char Int -> (Ptr CInt -> IO a) -> IO a
    withMap m f = do
      arr <- newArray $ fromIntegral . fromJust . (flip M.lookup m) <$>
             ['A','C','T','G']
      res <- f arr
      free arr
      return res
      
build :: (Ptr (Vector CInt) -> IO (Ptr (Vector CInt))) -> S.Seq Int -> S.Seq Int
build cbuild bigN =
  unsafePerformIO $ do
    withSeq (fromIntegral <$> bigN) $ \bigN_ptr -> do
      l <- fromVector =<< cbuild bigN_ptr
      return $ fromIntegral <$> S.fromList l

buildBigL' = build C.build_big_l'
buildSmallL' = build C.build_small_l'

strongGoodSuffixShift :: (S.Seq Int, S.Seq Int) -> HS.Match -> Int
strongGoodSuffixShift (bigL', l') m =
  unsafePerformIO $ do
    withSeq (fromIntegral <$> bigL') $ \big_l'_ptr -> do
      withSeq (fromIntegral <$> l') $ \l'_ptr -> do
        cm <- C.new_match found index
        let res = C.strong_good_suffix_shift big_l'_ptr l'_ptr cm
        free cm
        return $ fromIntegral res
  where
    found = case m of HS.Match -> 1 ; _ -> 0
    index = fromIntegral $ case m of HS.Match -> 0 ; HS.Mismatch x -> x

  
  

--------------------------------------------------------------------------------
