module Matrices.MatrixSpec where

import Test.Hspec

import Matrices.Matrix
import Control.Arrow

spec::Spec
spec = do
  let is = [0..3]

  describe "Matrix" $ do
    it "addition" $ do
      let is = [0..1]
      let r1 = [((1, toInteger i+1), toInteger $ [0, 1] !! i) | i <- is]
      let r2 = [((2, toInteger i+1), toInteger $ [2, 3] !! i) | i <- is]
      let m1 = matrix 2 2 (r1 ++ r2)

      let s1 = [((1, toInteger i+1), toInteger $ [4, 5] !! i) | i <- is]
      let s2 = [((2, toInteger i+1), toInteger $ [6, -7] !! i) | i <- is]
      let m2 = matrix 2 2 (s1 ++ s2)

      let expected = matrix 2 2 [((1, 1), 4), ((1, 2), 6), ((2, 1), 8), ((2, 2), -4)]

      (m1 `matrixAdd` m2) `shouldBe` Just expected

  describe "Matrix" $ do
    it "scalar multiplication" $ do
      let r1 = [((1, toInteger i+1), toInteger $ [1, 2, -1, 6] !! i) | i <- is]
      let r2 = [((2, toInteger i+1), toInteger $ [3, 8, 9, 10] !! i) | i <- is]
      let r3 = [((3, toInteger i+1), toInteger $ [2, -1, 2, -2] !! i) | i <- is]

      let m1 = matrix 3 4 (r1 ++ r2 ++ r3)

      let s1 = [second ((- 2) *) e | e <- r1]
      let s2 = [second ((- 2) *) e | e <- r2]
      let s3 = [second ((- 2) *) e | e <- r3]
      
      let expected = matrix 3 4 (s1 ++ s2 ++ s3)
      
      m1 `scalarMultiplyBy` (-2) `shouldBe` expected

  describe "Matrix" $ do
    it "transpose" $ do
      let r1 = [((1, toInteger i+1), toInteger $ [1, 2, -1, 6] !! i) | i <- is]
      let r2 = [((2, toInteger i+1), toInteger $ [3, 8, 9, 10] !! i) | i <- is]
      let r3 = [((3, toInteger i+1), toInteger $ [2, -1, 2, -2] !! i) | i <- is]

      let m1 = matrix 3 4 (r1 ++ r2 ++ r3)

      let js = [0..2]
      let s1 = [((1, toInteger j+1), toInteger $ [1, 3, 2] !! j) | j <- js]
      let s2 = [((2, toInteger j+1), toInteger $ [2, 8, -1] !! j) | j <- js]
      let s3 = [((3, toInteger j+1), toInteger $ [-1, 9, 2] !! j) | j <- js]
      let s4 = [((4, toInteger j+1), toInteger $ [6, 10, -2] !! j) | j <- js]

      let expected = matrix 4 3 (s1 ++ s2 ++ s3 ++ s4)

      transpose m1 `shouldBe` expected

  describe "Matrix" $ do
    it "row" $ do
      let r1 = [((1, toInteger i+1), toInteger $ [1, 2, -1, 6] !! i) | i <- is]
      let r2 = [((2, toInteger i+1), toInteger $ [3, 2, -4, 1] !! i) | i <- is]
      let r3 = [((3, toInteger i+1), toInteger $ [2, -1, 0, -2] !! i) | i <- is]

      let m1 = matrix 3 4 (r1 ++ r2 ++ r3)

      m1 `row` 2 `shouldBe` [3, 2, -4, 1]

  describe "Matrix" $ do
    it "column" $ do
      let r1 = [((1, toInteger i+1), toInteger $ [1, 2, -1, 6] !! i) | i <- is]
      let r2 = [((2, toInteger i+1), toInteger $ [3, 2, -4, 1] !! i) | i <- is]
      let r3 = [((3, toInteger i+1), toInteger $ [2, -1, 0, -2] !! i) | i <- is]

      let m1 = matrix 3 4 (r1 ++ r2 ++ r3)

      m1 `column` 1 `shouldBe` [1, 3, 2]

  describe "Matrix" $ do
    it "matrix multiplication" $ do
      let r1 = [((1, toInteger i+1), toInteger $ [1, 2, -1, 6] !! i) | i <- is]
      let r2 = [((2, toInteger i+1), toInteger $ [3, 2, -4, 1] !! i) | i <- is]
      let r3 = [((3, toInteger i+1), toInteger $ [2, -1, 0, -2] !! i) | i <- is]

      let m1 = matrix 3 4 (r1 ++ r2 ++ r3)

      let js = [0..2]
      let s1 = [((1, toInteger j+1), toInteger $ [1, 3, 2] !! j) | j <- js]
      let s2 = [((2, toInteger j+1), toInteger $ [2, 4, -1] !! j) | j <- js]
      let s3 = [((3, toInteger j+1), toInteger $ [-1, 0, 1] !! j) | j <- js]
      let s4 = [((4, toInteger j+1), toInteger $ [5, -3, -2] !! j) | j <- js]

      let m2 = matrix 4 3 (s1 ++ s2 ++ s3 ++ s4)

      let ks = [0..2]
      let t1 = [((1, toInteger k+1), toInteger $ [36, -7, -13] !! k) | k <- ks]
      let t2 = [((2, toInteger k+1), toInteger $ [16, 14, -2] !! k) | k <- ks]
      let t3 = [((3, toInteger k+1), toInteger $ [-10, 8, 9] !! k) | k <- ks]

      let expected = matrix 3 3 (t1 ++ t2 ++ t3)

      m1 `matrixMult` m2 `shouldBe` Just expected

  describe "Matrix" $ do
    it "row interchange" $ do
      let r1 = [((1, toInteger i+1), toInteger $ [1, 2, -1, 6] !! i) | i <- is]
      let r2 = [((2, toInteger i+1), toInteger $ [3, 2, -4, 1] !! i) | i <- is]
      let r3 = [((3, toInteger i+1), toInteger $ [2, -1, 0, -2] !! i) | i <- is]

      let m1 = matrix 3 4 (r1 ++ r2 ++ r3)

      let s1 = [((1, toInteger i+1), toInteger $ [2, -1, 0, -2] !! i) | i <- is]
      let s2 = [((2, toInteger i+1), toInteger $ [3, 2, -4, 1] !! i) | i <- is]
      let s3 = [((3, toInteger i+1), toInteger $ [1, 2, -1, 6] !! i) | i <- is]

      let expected = matrix 3 4 (s1 ++ s2 ++ s3)

      rowInterchange 1 3 m1 `shouldBe` expected  
      
  describe "Matrix" $ do
    it "row scalar multiplication" $ do
      let r1 = [((1, toInteger i+1), toInteger $ [1, 2, -1, 6] !! i) | i <- is]
      let r2 = [((2, toInteger i+1), toInteger $ [3, 2, -4, 1] !! i) | i <- is]
      let r3 = [((3, toInteger i+1), toInteger $ [2, -1, 0, -2] !! i) | i <- is]

      let m1 = matrix 3 4 (r1 ++ r2 ++ r3)

      let s1 = [((1, toInteger i+1), toInteger $ [1, 2, -1, 6] !! i) | i <- is]
      let s2 = [((2, toInteger i+1), toInteger $ [6, 4, -8, 2] !! i) | i <- is]
      let s3 = [((3, toInteger i+1), toInteger $ [2, -1, 0, -2] !! i) | i <- is]

      let expected = matrix 3 4 (s1 ++ s2 ++ s3)

      rowScalarMultiply 2 2 m1 `shouldBe` expected
        
  describe "Matrix" $ do
    it "row linear combination" $ do
      let r1 = [((1, toInteger i+1), toInteger $ [1, 2, -1, 6] !! i) | i <- is]
      let r2 = [((2, toInteger i+1), toInteger $ [3, 2, -4, 1] !! i) | i <- is]
      let r3 = [((3, toInteger i+1), toInteger $ [2, -1, 0, -2] !! i) | i <- is]

      let m1 = matrix 3 4 (r1 ++ r2 ++ r3)

      let s1 = [((1, toInteger i+1), toInteger $ [10, 8, -13, 9] !! i) | i <- is]
      let s2 = [((2, toInteger i+1), toInteger $ [3, 2, -4, 1] !! i) | i <- is]
      let s3 = [((3, toInteger i+1), toInteger $ [2, -1, 0, -2] !! i) | i <- is]

      let expected = matrix 3 4 (s1 ++ s2 ++ s3)
            
      rowLinearCombination 2 3 1 m1 `shouldBe` expected  
      
  describe "Matrix" $ do
    it "column interchange" $ do
      let r1 = [((1, toInteger i+1), toInteger $ [1, 2, -1, 6] !! i) | i <- is]
      let r2 = [((2, toInteger i+1), toInteger $ [3, 2, -4, 1] !! i) | i <- is]
      let r3 = [((3, toInteger i+1), toInteger $ [2, -1, 0, -2] !! i) | i <- is]

      let m1 = matrix 3 4 (r1 ++ r2 ++ r3)

      let s1 = [((1, toInteger i+1), toInteger $ [1, -1, 2, 6] !! i) | i <- is]
      let s2 = [((2, toInteger i+1), toInteger $ [3, -4, 2, 1] !! i) | i <- is]
      let s3 = [((3, toInteger i+1), toInteger $ [2, 0, -1, -2] !! i) | i <- is]

      let expected = matrix 3 4 (s1 ++ s2 ++ s3)

      columnInterchange 2 3 m1 `shouldBe` expected

  describe "Matrix" $ do
    it "column scalar multiplication" $ do
      let r1 = [((1, toInteger i+1), toInteger $ [1, 2, -1, 6] !! i) | i <- is]
      let r2 = [((2, toInteger i+1), toInteger $ [3, 2, -4, 1] !! i) | i <- is]
      let r3 = [((3, toInteger i+1), toInteger $ [2, -1, 0, -2] !! i) | i <- is]

      let m1 = matrix 3 4 (r1 ++ r2 ++ r3)

      let s1 = [((1, toInteger i+1), toInteger $ [1, 6, -1, 6] !! i) | i <- is]
      let s2 = [((2, toInteger i+1), toInteger $ [3, 6, -4, 1] !! i) | i <- is]
      let s3 = [((3, toInteger i+1), toInteger $ [2, -3, 0, -2] !! i) | i <- is]

      let expected = matrix 3 4 (s1 ++ s2 ++ s3)

      columnScalarMultiply 2 3 m1 `shouldBe` expected  

  describe "Matrix" $ do
    it "column linear combination" $ do
      let r1 = [((1, toInteger i+1), toInteger $ [1, 2, -1, 6] !! i) | i <- is]
      let r2 = [((2, toInteger i+1), toInteger $ [3, 2, -4, 1] !! i) | i <- is]
      let r3 = [((3, toInteger i+1), toInteger $ [2, -1, 0, -2] !! i) | i <- is]

      let m1 = matrix 3 4 (r1 ++ r2 ++ r3)

      let s1 = [((1, toInteger i+1), toInteger $ [7, 2, -1, 6] !! i) | i <- is]
      let s2 = [((2, toInteger i+1), toInteger $ [9, 2, -4, 1] !! i) | i <- is]
      let s3 = [((3, toInteger i+1), toInteger $ [-1, -1, 0, -2] !! i) | i <- is]

      let expected = matrix 3 4 (s1 ++ s2 ++ s3)
            
      columnLinearCombination 2 3 1 m1 `shouldBe` expected  
      