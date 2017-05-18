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

appendTCCmds :: TestCase -> TestCase -> TestCase
appendTCCmds (TestCase n xs) (TestCase _ ys) = TestCase n (xs ++ ys)

cmdGen :: Int -> Gen Cmd
cmdGen n = do
  x <- choose (1, n)
  y <- choose (1, n) `suchThat` (/= x)
  cmd <- frequency [(3, pure L),(3, pure C),(3, pure Q)]
  return (cmd x y)

cmdListGen :: Int -> Int -> Gen [Cmd]
cmdListGen cmdNum forestSize = resize cmdNum $ listOf1 $ cmdGen forestSize

instance Arbitrary TestCase where
  arbitrary = do
    n <- (getPositive <$> arbitrary) `suchThat` (\x -> 5 < x && x <= 30)

    cmds <- cmdListGen cmdNum forestSize
    return (TestCase { forestSize = n , commands = reverse (X : cmds)})

testCase :: TestCase -> String -> IO String
testCase tc exec = do
  let cmd = "cbits/link-cut/" ++ exec
  readProcess cmd [] (show tc)

prop_meetsSpec tc = monadicIO $ do
  expected <- run $ testCase tc "lc_spec"
  actual <- run $ testCase tc "lc"
  assert (actual == expected)

test = do
  putStrLn ""
  quickCheckWith stdArgs { maxSuccess = 10000 } prop_meetsSpec

linearTreeTC :: Int -> TestCase
linearTreeTC n = TestCase n $ take (n - 1) (zipWith L [1..] [2..])

linearTreeWithCutTC :: Int -> TestCase
linearTreeWithCutTC n = appendTCCmds (linearTreeTC n) (TestCase 1 [C x (x + 1)])
  where x = n `div` 2

main = do
  putStrLn ""
  let gen = (arbitrary :: Gen TestCase)
  (print . forestSize) =<< generate (gen `suchThat` (\(TestCase n _) -> n > 15))
