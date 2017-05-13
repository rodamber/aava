{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.Process

data Cmd = L Int Int
         | C Int Int
         | Q Int Int
         | X
         deriving (Show, Eq)

data TestCase = TestCase { forestSize :: Int
                         , commands :: [Cmd]
                         } deriving (Eq)

instance Show TestCase where
  show (TestCase n cmds) = show n ++ "\n" ++ unlines (map show cmds)

cmdGen :: Int -> Gen Cmd
cmdGen n = do
  x <- choose (1, n)
  y <- choose (1, n) `suchThat` (/= x)
  cmd <- frequency [(3, pure L),(3, pure C),(3, pure Q)]
  return (cmd x y)

instance Arbitrary TestCase where
  arbitrary = do
    n <- (getPositive <$> arbitrary) `suchThat` (\x -> 5 < x && x <= 30)
    cmds <- resize 30 $ listOf1 $ cmdGen n
    return (TestCase { forestSize = n , commands = reverse (X : cmds)})

testCase :: TestCase -> String -> IO String
testCase tc exec = do
  let cmd = "cbits/link-cut/" ++ exec
  readProcess cmd [] (show tc)

prop_meetsSpec tc = monadicIO $ do
  expected <- run $ testCase tc "lc_spec"
  actual <- run $ testCase tc "lc"
  assert (actual == expected)

main = do
  putStrLn ""
  quickCheckWith stdArgs { maxSuccess = 10000 } prop_meetsSpec
