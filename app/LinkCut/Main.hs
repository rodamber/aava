{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

foreign import ccall "main_link_cut"
    c_mainLinkCut :: IO ()

main = do
  c_mainLinkCut
