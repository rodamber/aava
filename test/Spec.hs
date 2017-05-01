{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Strict                    #-}
{-# LANGUAGE ViewPatterns              #-}

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

import Lib

import qualified FFI as C
import FFI (Result, Vector)

import qualified Haskell as HS
import Haskell (Txt, Pat, Input(..), Output(..))

--------------------------------------------------------------------------------
-- QuickCheck
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

data BadCharInput = BadCharInput T.Text Int Char
  deriving (Eq, Show)

instance Arbitrary BadCharInput where
  arbitrary = do
    txt <- arbitrary
    int <- choose (1, T.length txt)
    char <- elements ['A','C','T','G']
    return $ BadCharInput txt int char

--------------------------------------------------------------------------------
-- General
--------------------------------------------------------------------------------

matchSpec = prop "matches the haskell spec"

prop_searchPositions search input =
  positions (search input) == positions (HS.naive input)

matchTests search = do
  prop "matches when the text and the pattern are equal" $
    \(EqualInput x) -> positions (search x) `shouldBe` [0]
  professorTests search

test_search msg search =
  describe msg $ do
    matchTests search
    -- matchSpec $ prop_searchPositions search

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

--------------------------------------------------------------------------------
-- Z Algorithm
--------------------------------------------------------------------------------

prop_zMatchCount t x = HS.zMatchCount t x x == T.length (T.drop (x - 1) t)

test_zMatchCount = do
  describe "zMatchCount" $ do
    prop "..." $ prop_zMatchCount

    let s = "aabcaabxaaz"
    it "..." $ do
      HS.zMatchCount s 1 5 `shouldBe` 3
      HS.zMatchCount s 1 9 `shouldBe` 2

prop_reverseChar t = reverseChar t == T.reverse t

test_reverseChar = prop "reverses text correctly" prop_reverseChar

prop_zAlgorithm t = zAlgorithm t == HS.zAlgorithmSpec t
prop_reverseZAlgorithm t = reverseZAlgorithm t == HS.reverseZAlgorithmSpec t

test_zAlgorithm = do
  describe "zAlgorithm" $ do

    describe "normal" $ do
      matchSpec prop_zAlgorithm

      prop "always has the same length as its input" $ \t ->
        length (HS.zAlgorithmSpec t) == T.length t

    describe "reverse" $ do
      matchSpec prop_reverseZAlgorithm

      prop "always has the same length as its input" $ \t ->
        length (HS.reverseZAlgorithmSpec t) == T.length t

      it "..." $ do
        HS.reverseZAlgorithmSpec "cabdabdab" `shouldBe` [0,0,2,0,0,5,0,0,0]
        HS.reverseZAlgorithmSpec "cabdabdab" `shouldBe` [0,0,2,0,0,5,0,0,0]

      describe "reverseChar" $ do
        test_reverseChar

--------------------------------------------------------------------------------
-- Boyer Moore
--------------------------------------------------------------------------------

prop_badCharPreprocessingMatchesSpec t =
  HS.badCharPreprocessingSpec t == badCharPreprocessing t

prop_badCharShiftMatchesSpec (BadCharInput t i c) =
  HS.badCharShiftSpec r i c == badCharShift r i c
  where r = badCharPreprocessing t

test_badCharRule =
  describe "Bad Char Rule" $ do
    describe "Processing" $ do
      matchSpec prop_badCharPreprocessingMatchesSpec
    describe "Shift" $ do
      matchSpec prop_badCharShiftMatchesSpec

prop_L'HasSameLengthAsPattern buildL t =
  let bigN = S.fromList $ HS.reverseZAlgorithmSpec t
  in T.length t == length (buildL bigN)

prop_bigL'HasSameLengthAsPattern = prop_L'HasSameLengthAsPattern HS.buildBigL'Spec
prop_smallL'HasSameLengthAsPattern = prop_L'HasSameLengthAsPattern HS.buildSmallL'Spec

test_strongGoodSuffixRule =
  describe "Strong Good Suffix Rule" $ do
    describe "unit tests" $ do
      let s = "AAAAAAAAA"
      let bigN = S.fromList $ HS.reverseZAlgorithmSpec s

      specify "test 01" $ do
        HS.buildBigL'Spec   bigN `shouldBe` S.fromList [0,8,7,6,5,4,3,2,1]
      specify "test 02" $ do
        HS.buildSmallL'Spec bigN `shouldBe` S.fromList [8,8,7,6,5,4,3,2,1]

      let s = "cabdabdab"
      let bigN = S.fromList $ HS.reverseZAlgorithmSpec s

      specify "test 03" $ do
        HS.buildBigL'Spec   bigN `shouldBe` S.fromList [0,0,0,0,6,0,0,3,0]
      specify "test 04" $ do
        HS.buildSmallL'Spec bigN `shouldBe` S.fromList [0,0,0,0,0,0,0,0,0]

    describe "Big L'" $ do
      prop "tables and input have the same length" $ do
        prop_bigL'HasSameLengthAsPattern
      matchSpec prop_bigL'MatchesSpec

    describe "Small l'" $ do
      prop "tables and input have the same length" $ do
        prop_smallL'HasSameLengthAsPattern
      matchSpec prop_smallL'MatchesSpec

prop_bigL'MatchesSpec s = HS.buildBigL'Spec bigN == buildBigL' bigN
  where bigN = S.fromList $ reverseZAlgorithm s
prop_smallL'MatchesSpec s = HS.buildSmallL'Spec bigN == buildSmallL' bigN
  where bigN = S.fromList $ reverseZAlgorithm s

--------------------------------------------------------------------------------
-- Haskell naive spec
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main = do
  -- traverse_ (quickCheckWith stdArgs { maxSuccess = 200 } . prop_searchPositions)
  --   [ naive
  --   , knuthMorrisPratt
  --   , boyerMoore
  --   ]

  -- traverse_ (quickCheckWith stdArgs { maxSuccess = 200 })
  --   [  prop_bigL'MatchesSpec
  --    , prop_smallL'MatchesSpec
  --   ]

  -- hspec $ do
  --   describe "Haskell" $ do
  --     test_indexedTails
  --     test_matchIndex
  --     describe "naive" $ matchTests HS.naive

  --     test_zMatchCount

  --   describe "C" $ do
  --     test_search "naive" naive
  --     test_search "knuth_morris_pratt" knuthMorrisPratt


  -- quickCheckWith stdArgs { maxSuccess = 100 } $ prop_searchPositions boyerMoore

  hspec $ do
    describe "Boyer Moore" $ do
      test_badCharRule
      test_zAlgorithm
      test_strongGoodSuffixRule
      test_search "boyer_moore" boyerMoore

    describe "Boyer Moore Unit Tests" $ do
      let test (n,input) = it (show n) $ boyerMoore input `shouldBe` HS.naive input
      traverse_ test $ zip [0..]
        [ Input "CCTTT" "CTTT"
        , Input "AAAAA" "A"
        , Input {inputText = "CGGGCCACAGCTGCTTCTCTTCAAATGGACGCCTACGCGAATTACATGAGCAGATG",
                inputPattern = "A"}
        , Input {inputText = "TCAAA", inputPattern = "A"}
        , Input {inputText = "AGACTGAATCCCTGCAACATTAAGG", inputPattern = "A"}
        , Input {inputText = "GTTGAGCCTTCACGGTAG", inputPattern = "T"}
        , Input {inputText = "ATTTAACAAGG", inputPattern = "AGG"}
        , Input {inputText = "GG", inputPattern = "G"}
        ]


