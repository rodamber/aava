{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Strict                   #-}
module FFI where

import Foreign
import Foreign.C.Types
import Foreign.C.String

data Result
data Vector a

type Search =  Ptr (Vector CChar) -- text
            -> Ptr (Vector CChar) -- pattern
            -> IO (Ptr Result)

left_right = 1
right_left = -1

foreign import ccall unsafe "get_array_int"
    get_array_int :: Ptr (Vector CInt) -> Ptr CInt

foreign import ccall unsafe "get_size_int"
    get_size_int :: Ptr (Vector CInt) -> CInt

foreign import ccall unsafe "get_array_char"
    get_array_char :: Ptr (Vector CChar) -> Ptr CChar

foreign import ccall unsafe "get_size_char"
    get_size_char :: Ptr (Vector CChar) -> CInt

foreign import ccall unsafe "from_array_int"
    from_array_int :: Ptr CInt -> CInt -> IO (Ptr (Vector CInt))

foreign import ccall unsafe "from_array_char"
    from_array_char :: Ptr CChar -> CInt -> IO (Ptr (Vector CChar))

foreign import ccall unsafe "new_string"
    new_string :: CString -> IO (Ptr (Vector CChar))

foreign import ccall unsafe "&free_vector_char"
    finalizerVectorChar :: FunPtr (Ptr (Vector CChar) -> IO ())

foreign import ccall unsafe "&free_vector_int"
    finalizerVectorInt :: FunPtr (Ptr (Vector CInt) -> IO ())

foreign import ccall unsafe "dynamic"
    mkFreeVectorInt :: (FunPtr (Ptr (Vector CInt) -> IO ()))
                    -> (Ptr (Vector CInt) -> IO ())

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
    ncmp :: CString -> CString -> CInt -> CInt -> Ptr Result -> CInt

foreign import ccall unsafe "reverse_char"
    reverse_char :: Ptr (Vector CChar) -> IO (Ptr (Vector CChar))

foreign import ccall unsafe "match_count"
    match_count :: CString -> CString -> CInt

foreign import ccall unsafe "z_algorithm"
    z_algorithm :: Ptr (Vector CChar) -> IO (Ptr (Vector CInt))

foreign import ccall unsafe "reverse_z_algorithm"
    reverse_z_algorithm :: Ptr (Vector CChar) -> IO (Ptr (Vector CInt))

foreign import ccall unsafe "compute_prefix_function"
    compute_prefix_function :: Ptr (Vector CChar) -> IO (Ptr (Vector CInt))

foreign import ccall unsafe "bad_char_preprocessing"
    bad_char_preprocessing :: Ptr (Vector CChar) -> IO (Ptr CInt)

foreign import ccall unsafe "bad_char_shift"
    bad_char_shift :: Ptr CInt -> CInt -> CChar

foreign import ccall unsafe "build_big_l_prime"
    build_big_l' :: Ptr (Vector CInt) -> IO (Ptr (Vector CInt))

foreign import ccall unsafe "build_small_l_prime"
    build_small_l' :: Ptr (Vector CInt) -> IO (Ptr (Vector CInt))

foreign import ccall unsafe "strong_good_suffix_shift"
    strong_good_suffix_shift :: Ptr (Ptr (Vector CInt)) -> CInt -> CInt

foreign import ccall unsafe "main_"
    main_ :: IO ()
