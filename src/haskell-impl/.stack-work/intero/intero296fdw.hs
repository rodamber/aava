module Haskell where

import Control.Monad.State

import Lib

match :: Txt -> Pat -> (Pat, Int)
match txt pat = fmap (length) $ break (uncurry (==)) (zip txt pat)



-- naive :: Search
-- naive txt pat = execState (go txt pat) (Output [] 0)
--   where
--     go :: Txt -> Pat -> State Output ()

