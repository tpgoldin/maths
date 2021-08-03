module Matrices.RowEchelonFormTransformationSpec where

import Test.Hspec

import Matrices.Matrix
import Matrices.RowEchelonFormTransformation

buildRow :: [Int] -> [a] -> [((Integer, Integer), a)]
buildRow is xs = [((1, toInteger i+1), xs !! i) | i <- is]

spec::Spec
spec = do
  describe "Row echelon form transformation" $ do
    it "find the first column containing a nonzero element" $ do
      let is = [0..3]
      let r1 = buildRow is [0, 0, -1, 6]
      let r2 = buildRow is [0, 8, 9, 10]
      let r3 = buildRow is [0, -1, 2, -2]

      let m1 = matrix 3 4 (r1 ++ r2 ++ r3)

      let s1 = buildRow is [0, 8, 9, 10]
      let s2 = buildRow is [0, 0, -1, 6]
      let s3 = buildRow is [0, -1, 2, -2]

      let expected = matrix 3 4 (s1 ++ s2 ++ s3)

      transformToRowEchelonForm m1 `shouldBe` expected