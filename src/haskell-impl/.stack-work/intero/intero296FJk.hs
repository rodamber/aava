{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module FFI where

import Foreign
import Foreign.C.Types
import Foreign.C.String

data Result
data Vector a

left_right = 1
right_left = -1

foreign import ccall unsafe "new_string"
    c_new_string         :: CString -> IO (Ptr (Vector Char))
foreign import ccall unsafe "&free_vector_char"
    finalizerVectorChar  :: FunPtr (Ptr (Vector Char) -> IO ())
foreign import ccall unsafe "&free_vector_int"
    finalizerVectorInt   :: FunPtr (Ptr (Vector Int) -> IO ())
foreign import ccall unsafe "&free_result"
    finalizerResult      :: FunPtr (Ptr Result -> IO ())
foreign import ccall unsafe "get_positions"
    c_get_positions      :: Ptr Result -> IO (Ptr CInt)
foreign import ccall unsafe "get_positions_size"
    c_get_positions_size :: Ptr Result -> CInt
foreign import ccall unsafe "get_comparisons"
    c_get_comparisons    :: Ptr Result -> CInt
foreign import ccall unsafe "naive"
    c_naive
        :: Ptr (Vector Char) -- text
        -> Ptr (Vector Char) -- pattern
        -> IO (Ptr (Result))
foreign import ccall unsafe "knuth_morris_pratt"
    c_knuth_morris_pratt
        :: Ptr (Vector Char) -- text
        -> Ptr (Vector Char) -- pattern
        -> IO (Ptr (Result))
foreign import ccall unsafe "boyer_moore"
    c_boyer_moore
        :: Ptr (Vector Char) -- text
        -> Ptr (Vector Char) -- pattern
        -> IO (Ptr (Result))
foreign import ccall unsafe "ncmp"
    c_ncmp                    :: CString -> CString -> CInt -> CInt -> Ptr (Result)
foreign import ccall unsafe "match_count"
    c_match_count             :: CString -> CString -> CInt
foreign import ccall unsafe "z_algorithm"
    c_z_algorithm             :: Ptr (Vector Char) -> Ptr (Vector Int)
foreign import ccall unsafe "reverse_z_algorithm"
    c_reverse_z_algorithm     :: Ptr (Vector Char) -> Ptr (Vector Int)
foreign import ccall unsafe "compute_prefix_function"
    c_compute_prefix_function :: Ptr (Vector Char) -> Ptr (Vector Int)
foreign import ccall unsafe "bad_char_preprocessing"
    c_bad_char_preprocessing  :: Ptr (Vector Char) -> CInt
foreign import ccall unsafe "bad_char_shift"
    c_bad_char_shift          :: Ptr CInt -> CInt -> CChar
foreign import ccall unsafe "strong_good_suffix_preprocessing"
    c_strong_good_suffix_preprocessing :: Ptr (Vector Char) -> Ptr (Vector Int)
foreign import ccall unsafe "strong_good_suffix_shift"
    c_strong_good_suffix_shift :: Ptr (Ptr (Vector Int)) -> CInt -> Ptr Result

