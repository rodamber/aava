{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Strict                   #-}
module FFI where

import Foreign
import Foreign.C.Types
import Foreign.C.String

data Match
type MatchPtr = Ptr Match
data Result
type ResultPtr = Ptr Result
data Vector a
type VectorPtr a = Ptr (Vector a)

type Search =  VectorPtr CChar -- text
            -> VectorPtr CChar -- pattern
            -> IO ResultPtr

left_right = 1
right_left = -1

foreign import ccall unsafe "get_array_int"
    get_array_int :: VectorPtr CInt -> Ptr CInt

foreign import ccall unsafe "get_size_int"
    get_size_int :: VectorPtr CInt -> CInt

foreign import ccall unsafe "get_array_char"
    get_array_char :: VectorPtr CChar -> Ptr CChar

foreign import ccall unsafe "get_size_char"
    get_size_char :: VectorPtr CChar -> CInt

foreign import ccall unsafe "from_array_int"
    from_array_int :: Ptr CInt -> CInt -> IO (VectorPtr CInt)

foreign import ccall unsafe "from_array_char"
    from_array_char :: Ptr CChar -> CInt -> IO (VectorPtr CChar)

foreign import ccall unsafe "new_string"
    new_string :: CString -> IO (VectorPtr CChar)

foreign import ccall unsafe "&free_vector_char"
    finalizerVectorChar :: FunPtr (VectorPtr CChar -> IO ())

foreign import ccall unsafe "&free_vector_int"
    finalizerVectorInt :: FunPtr (VectorPtr CInt -> IO ())

foreign import ccall unsafe "dynamic"
    mkFreeVectorInt :: (FunPtr (VectorPtr CInt -> IO ()))
                    -> (VectorPtr CInt -> IO ())

foreign import ccall unsafe "&free_result"
    finalizerResult :: FunPtr (ResultPtr -> IO ())

foreign import ccall unsafe "get_positions"
    get_positions :: ResultPtr -> IO (Ptr CInt)

foreign import ccall unsafe "get_positions_size"
    get_positions_size :: ResultPtr -> CInt

foreign import ccall unsafe "get_comparisons"
    get_comparisons :: ResultPtr -> CInt

foreign import ccall unsafe "naive"
    naive :: Search

foreign import ccall unsafe "knuth_morris_pratt"
    knuth_morris_pratt :: Search

foreign import ccall unsafe "boyer_moore"
    boyer_moore :: Search

foreign import ccall unsafe "new_match"
    new_match :: CInt -> CInt -> IO MatchPtr

foreign import ccall unsafe "match_found"
    match_found :: MatchPtr -> CInt

foreign import ccall unsafe "match_index"
    match_index :: MatchPtr -> CInt

foreign import ccall unsafe "ncmp"
    ncmp :: CString -> CString -> CInt -> CInt -> ResultPtr -> CInt

foreign import ccall unsafe "reverse_char"
    reverse_char :: VectorPtr CChar -> IO (VectorPtr CChar)

foreign import ccall unsafe "match_count"
    match_count :: CString -> CString -> CInt

foreign import ccall unsafe "z_algorithm"
    z_algorithm :: VectorPtr CChar -> IO (VectorPtr CInt)

foreign import ccall unsafe "reverse_z_algorithm"
    reverse_z_algorithm :: VectorPtr CChar -> IO (VectorPtr CInt)

foreign import ccall unsafe "compute_prefix_function"
    compute_prefix_function :: VectorPtr CChar -> IO (VectorPtr CInt)

foreign import ccall unsafe "bad_char_preprocessing"
    bad_char_preprocessing :: VectorPtr CChar -> IO (Ptr CInt)

foreign import ccall unsafe "bad_char_shift"
    bad_char_shift :: Ptr CInt -> MatchPtr -> CChar -> CInt

foreign import ccall unsafe "build_big_l_prime"
    build_big_l' :: VectorPtr CInt -> IO (VectorPtr CInt)

foreign import ccall unsafe "build_small_l_prime"
    build_small_l' :: VectorPtr CInt -> IO (VectorPtr CInt)

foreign import ccall unsafe "strong_good_suffix_shift"
    strong_good_suffix_shift :: VectorPtr CInt
                             -> VectorPtr CInt -> MatchPtr -> CInt

foreign import ccall unsafe "main_"
    main_ :: IO ()
