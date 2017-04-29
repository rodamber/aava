{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ViewPatterns        #-}

import Data.List
import Data.Maybe
import qualified Data.Text as T

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import Lib

import qualified C
import C (Result, Vector)

import qualified Haskell as HS
import Haskell (Txt, Pat, Input(..), Output(..))


instance Arbitrary Input where
  arbitrary = do
    let g = listOf1 (elements ['A', 'C', 'T', 'G'])
    txt <- g
    let len = length txt
    pat <- g `suchThat` \p -> length p <= len
    return $ Input (T.pack txt) (T.pack pat)

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

prop_searchPositionsSpec search input =
  positions (match search input) == positions (HS.naive input)

matchSpec cSearch =
  it "matches the haskell spec" $ property $ prop_searchPositionsSpec cSearch

professorTests s = do
  it "passes test 01" $ do
    positions (match s (Input "TCGCAGGGCG" "TC")) `shouldBe` [0]
  it "passes test 02" $ do
    positions (match s (Input "AAAAAAAAAA" "AAA")) `shouldBe` [0,1,2,3,4,5,6,7]
  it "passes test 03" $ do
    positions (match s (Input "AGGTACCCAT" "CA")) `shouldBe` [7]
  -- test 04 is the same as test 02
  it "passes test 05" $ do
    positions (match s (Input "GCCCAAAGAC" "CA")) `shouldBe` [3]
  -- test 06 is the same as test 02

matchTests s = do
  it "matches when the text and the pattern are equal" $ property $ \x ->
    positions (match s x) `shouldBe` [0]
  professorTests s

test_indexedTails =
 describe "indexedTails" $ do
   it "has the same length as the input" $ property $ \x ->
     T.length x == length (HS.indexedTails x)

   it "has proper indices" $ property $ \x ->
     fmap fst (HS.indexedTails x) == [0 .. T.length x - 1]

   it "produces correct tails" $ property $ \x ->
     fmap snd (HS.indexedTails x) == fmap T.pack (init (tails (T.unpack x)))

test_matchIndex =
  describe "matchIndex" $ do
    it "returns a Just value if the objects match" $ property $ \x ix ->
      not (T.null x) ==> isJust (HS.matchIndex x (ix, x))

    it "always returns the int passed in the pair" $ property $ \x y ix ->
      case HS.matchIndex x (ix, y) of
        Just ix' -> ix == ix'
        Nothing -> True

main = do
  let txt = "C"
  let pat = "C"

  putStr "C naive:" >> print (match C.naive (Input txt pat))
  putStr "C knuth_morris_pratt:" >> print (match C.naive (Input txt pat))
  putStr "C boyer_moore:" >> print (match C.boyer_moore (Input txt pat))
  putStr "HS:" >> print (match HS.naive (Input txt pat))

  hspec $ do
    describe "Haskell" $ do
      test_indexedTails
      test_matchIndex
      describe "naive" $ matchTests HS.naive

    describe "C" $ do
      describe "naive" $ do
        matchTests C.naive
        matchSpec C.naive

      describe "knuth-morris-pratt" $ do
        matchTests C.knuth_morris_pratt
        matchSpec C.knuth_morris_pratt

      describe "boyer-moore" $ do
        matchTests C.boyer_moore
        matchSpec C.boyer_moore

