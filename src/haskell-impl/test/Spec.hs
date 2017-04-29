{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns        #-}

import Data.List
import Data.Maybe
import qualified Data.Text as T

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import Lib

import qualified C
import C (Result, Vector)

import qualified Haskell as HS
import Haskell (Txt, Pat, Input(..), Output(..))

--------------------------------------------------------------------------------

myGen = listOf1 (elements ['A', 'C', 'T', 'G'])

instance Arbitrary Input where
  arbitrary = do
    txt <- myGen
    let len = length txt
    pat <- myGen `suchThat` \p -> length p <= len
    return $ Input (T.pack txt) (T.pack pat)

newtype EqualInput = EqualInput Input
  deriving (Show, Eq)

instance Arbitrary EqualInput where
  arbitrary = do
    txt <- T.pack <$> myGen
    return $ EqualInput $ Input txt txt
    
instance Arbitrary T.Text where
  arbitrary = T.pack <$> myGen

--------------------------------------------------------------------------------

matchSpec = it "matches the haskell spec" . property

prop_zMatchCount t x = HS.zMatchCount t x x == T.length (T.drop (x - 1) t)

test_zMatchCount = do
  describe "zMatchCount" $ do
    prop "..." $ prop_zMatchCount

    let s = "aabcaabxaaz"
    it "..." $ HS.zMatchCount s 1 5 `shouldBe` 3
    it "..." $ HS.zMatchCount s 1 9 `shouldBe` 2

prop_reverseChar t = reverseChar t == T.reverse t

test_reverseChar = prop "reverses text correctly" prop_reverseChar

prop_zAlgorithm t = zAlgorithm t == HS.zAlgorithmSpec t
prop_reverseZAlgorithm t = reverseZAlgorithm t == HS.reverseZAlgorithmSpec t

test_zAlgorithm = do
  describe "zAlgorithm" $ do
    describe "normal" $ do
      matchSpec prop_zAlgorithm

    describe "reverse" $ do
      matchSpec prop_reverseZAlgorithm

      describe "reverseChar" $ do
        test_reverseChar

prop_searchPositions search input =
  positions (search input) == positions (HS.naive input)

professorTests search = do
  it "passes test 01" $ do
    positions (search (Input "TCGCAGGGCG" "TC")) `shouldBe` [0]
  it "passes test 02" $ do
    positions (search (Input "AAAAAAAAAA" "AAA")) `shouldBe` [0,1,2,3,4,5,6,7]
  it "passes test 03" $ do
    positions (search (Input "AGGTACCCAT" "CA")) `shouldBe` [7]
  -- test 04 is the same as test 02
  it "passes test 05" $ do
    positions (search (Input "GCCCAAAGAC" "CA")) `shouldBe` [3]
  -- test 06 is the same as test 02

matchTests search = do
  prop "matches when the text and the pattern are equal" $
    \(EqualInput x) -> positions (search x) `shouldBe` [0]
  professorTests search

test_indexedTails =
 describe "indexedTails" $ do
   prop "has the same length as the input" $ \x ->
     T.length x == length (HS.indexedTails x)

   prop "has proper indices" $ \x ->
     fmap fst (HS.indexedTails x) == [0 .. T.length x - 1]

   prop "produces correct tails" $ \x ->
     fmap snd (HS.indexedTails x) == fmap T.pack (init (tails (T.unpack x)))

test_matchIndex =
  describe "matchIndex" $ do
    prop "returns a Just value if the objects match" $ \x ix ->
      not (T.null x) ==> isJust (HS.matchIndex x (ix, x))

    prop "always returns the int passed in the pair" $ \x y ix ->
      case HS.matchIndex x (ix, y) of
        Just ix' -> ix == ix'
        Nothing -> True

test_search msg search =
  describe msg $ do
    matchTests search
    matchSpec $ prop_searchPositions search

--------------------------------------------------------------------------------

main = do
  -- let txtKMP = "CAGCTCTCTTTGCGAAGTCTGACCGCAGTTGGATGGAGTA"
  -- let patKMP = "GAG"
  -- putStr "C knuth_morris_pratt:" >> print (match C.naive (Input txtKMP patKMP))
  -- putStr "HS:" >> print (match HS.naive (Input txtKMP patKMP))

  -- let txt = "TTTAGTCCTTG"
  -- let txtBMBM = "TT"
  -- putStr "C boyer_moore:" >> print (match C.boyer_moore (Input txt txtBMBM))
  -- putStr "HS:" >> print (match HS.naive (Input txt txtBMBM))

  let s = "TCTC"
  putStrLn "C: "  >> print (reverseZAlgorithm s)
  putStrLn "HS: " >> print (HS.reverseZAlgorithmSpec s)

  hspec $ do
    describe "Haskell" $ do
      test_indexedTails
      test_matchIndex
      describe "naive" $ matchTests HS.naive

      test_zMatchCount

    describe "C" $ do
      test_search "naive" naive
      test_search "knuth_morris_pratt" knuthMorrisPratt

      describe "Boyer Moore" $ do
        test_zAlgorithm
        test_search "boyer_moore" boyerMoore

