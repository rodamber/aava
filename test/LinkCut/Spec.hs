{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Control.Monad
import Data.Foldable
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Text as T

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Monadic

--------------------------------------------------------------------------------
-- FFI                                                                        --
--------------------------------------------------------------------------------

foreign import ccall unsafe "init"
    c_init :: CInt -> IO ()

start :: Int -> IO ()
start = c_init . fromIntegral

foreign import ccall unsafe "finally"
    c_finally :: IO ()

finally :: IO ()
finally = c_finally

foreign import ccall unsafe "Link"
    c_Link :: CInt -> CInt -> IO ()

link :: Int -> Int -> IO ()
link u v = c_Link (fromIntegral u) (fromIntegral v)

foreign import ccall unsafe "Cut"
    c_Cut :: CInt -> CInt -> IO ()

cut :: Int -> Int -> IO ()
cut u v = c_Cut (fromIntegral u) (fromIntegral v)

foreign import ccall unsafe "ConnectedQ"
    c_ConnectedQ :: CInt -> CInt -> IO Bool

connected :: Int -> Int -> IO Bool
connected u v = c_ConnectedQ (fromIntegral u) (fromIntegral v)

foreign import ccall unsafe "pstate"
    c_pstate :: IO ()

pstate = c_pstate

withNodes :: Int -> IO a -> IO a
withNodes n f = do
  start n
  res <- f
  finally
  return res

-- data Action a = Action (Int -> Int -> IO a) Int Int
-- data Actions = Actions { nodeCount :: Int , actions :: [Action a] }

data Action = Action { actionChar :: Char, actionU :: Int, actionV :: Int}
  deriving (Eq, Show)
data Actions = Actions { nodeCount :: Int , actions :: [Action] }
  deriving (Eq, Show)

instance Arbitrary Actions where
  arbitrary = do
    n <- arbitrarySizedNatural `suchThat` (> 10)
    cs <- listOf1 $ elements ['l', 'c', 'q']
    as <- forM cs $ \c -> do
      u <- choose (0, n-2)
      v <- choose (u+1, n-1)
      return $ Action c u v
    return $ Actions n as

newtype LinkConActions = LinkConActions (Actions, Int, Int)
  deriving (Eq, Show)

instance Arbitrary LinkConActions where
  arbitrary = do
    (Actions n as) <- arbitrary
    u <- choose (0, n-2)
    v <- choose (u+1, n-1)
    let as = filter (\(Action c _ _) -> c /= 'c') as
    return $ LinkConActions (Actions n as, u, v)

fun :: Char -> (Int -> Int -> IO ())
fun c = case c of 'l' -> link
                  'c' -> cut
                  'q' -> \u -> Control.Monad.void . connected u

data Simple = Simple Int Int Int deriving (Show, Eq)

instance Arbitrary Simple where
  arbitrary = do
    n <- arbitrarySizedNatural `suchThat` (> 10)
    u <- choose(0, n - 2)
    v <- choose(u + 1, n - 1)
    return (Simple n u v)

prop_cutAfterLinkIsNotConnected (Simple n u v) =
  monadicIO $ do
    c <- run $ withNodes n $ do
      c <- do
        link u v
        -- pstate
        cut u v
        -- pstate
        connected u v
      -- pstate
      return c
    assert (not c)

prop_linkAfterCutIsConnected (Simple n u v) =
  monadicIO $ do
    c <- run $ withNodes n $ do
      c <- do
        cut u v
        pstate
        link u v
        pstate
        connected u v
      pstate
      return c
    assert c


-- prop_generalizedLinkedAreConnected (Actions n as) (Positive u) (Positive v) =
--   monadicIO $ do
--     -- run $ putStrLn $ "n: " ++ show n ++ ", u: " ++ show u ++ ", v: " ++ show v
--     pre (u < v && v < n)
--     res <- run $ withNodes n $ do
--       link u v
--       connected u v

--     assert res

-- prop_generalizedLinkedAreConnected (LinkConActions ((Actions n as), u, v)) =
--   monadicIO $ do
--     run $ putStrLn $ "n: " ++ show n ++ ", u: " ++ show u ++ ", v: " ++ show v
--     res <- run $ withNodes n $ do
--       link u v
--       -- forM_ as $ \(Action c w x) -> fun c w x
--       connected u v

--     assert res

-- prop_generalizedLinkedAreConnected

--------------------------------------------------------------------------------
--                                                                            --
--------------------------------------------------------------------------------

-- spec = do
--   describe "Link and Connect" $ do
--     prop "..." $ prop_generalizedLinkedAreConnected

return []
runtests = $quickCheckAll

main = do
  runtests
  -- hspec spec
