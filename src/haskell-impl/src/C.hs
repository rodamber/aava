{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module C where

import Foreign
import Foreign.C.Types
import Foreign.C.String

data Result
data Vector a

type Search =  Ptr (Vector Char) -- text
            -> Ptr (Vector Char) -- pattern
            -> IO (Ptr (Result))

left_right = 1
right_left = -1

foreign import ccall unsafe "new_string"
    new_string :: CString -> IO (Ptr (Vector Char))

foreign import ccall unsafe "&free_vector_char"
    finalizerVectorChar :: FunPtr (Ptr (Vector Char) -> IO ())

foreign import ccall unsafe "&free_vector_int"
    finalizerVectorInt :: FunPtr (Ptr (Vector Int) -> IO ())

foreign import ccall unsafe "&free_result"
    finalizerResult :: FunPtr (Ptr Result -> IO ())

foreign import ccall unsafe "get_positions"
    get_positions :: Ptr Result -> IO (Ptr CInt)

foreign import ccall unsafe "get_positions_size"
    get_positions_size :: Ptr Result -> CInt

foreign import ccall unsafe "get_comparisons"
    get_comparisons :: Ptr Result -> CInt

foreign import ccall unsafe "naive"
    naive :: Search

foreign import ccall unsafe "knuth_morris_pratt"
    knuth_morris_pratt :: Search

foreign import ccall unsafe "boyer_moore"
    boyer_moore :: Search

foreign import ccall unsafe "ncmp"
    ncmp :: CString -> CString -> CInt -> CInt -> Ptr (Result)

foreign import ccall unsafe "match_count"
    match_count :: CString -> CString -> CInt

foreign import ccall unsafe "z_algorithm"
    z_algorithm :: Ptr (Vector Char) -> Ptr (Vector Int)

foreign import ccall unsafe "reverse_z_algorithm"
    reverse_z_algorithm :: Ptr (Vector Char) -> Ptr (Vector Int)

foreign import ccall unsafe "compute_prefix_function"
    compute_prefix_function :: Ptr (Vector Char) -> Ptr (Vector Int)

foreign import ccall unsafe "bad_char_preprocessing"
    bad_char_preprocessing :: Ptr (Vector Char) -> CInt

foreign import ccall unsafe "bad_char_shift"
    bad_char_shift :: Ptr CInt -> CInt -> CChar

foreign import ccall unsafe "strong_good_suffix_preprocessing"
    strong_good_suffix_preprocessing :: Ptr (Vector Char) -> Ptr (Vector Int)

foreign import ccall unsafe "strong_good_suffix_shift"
    strong_good_suffix_shift :: Ptr (Ptr (Vector Int)) -> CInt -> Ptr Result
