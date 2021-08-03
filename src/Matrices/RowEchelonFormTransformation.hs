module Matrices.RowEchelonFormTransformation (
  transformToRowEchelonForm
) where

import Data.Array
import Matrices.Matrix

isNotZero :: Num a => Eq a => [a] -> Bool
isNotZero = any (/= 0)

selectNonZeroColumn :: Num a => Eq a => Matrix a -> Integer
selectNonZeroColumn aMatrix = cIndex
                                where c = m aMatrix
                                      selectedCols = filter (isNotZero . (aMatrix `column`)) [1..c]
                                      cIndex = if null selectedCols then 0 else head selectedCols

isNotZeroAt :: Num a => Eq a => Matrix a -> (Integer, Integer) -> Bool
isNotZeroAt aMatrix e = if (snd e == 0)
                        then False
                        else ((cells aMatrix) ! e) /= 0

selectRowWithNonZeroColumn :: Num a => Eq a => Matrix a -> [Integer] -> Integer -> Integer
selectRowWithNonZeroColumn aMatrix rIndices c = rIndex
                                                where rs = [r | r <- rIndices, (isNotZeroAt aMatrix (r, c))]
                                                      rIndex = if null rs then 0 else head rs

iterateOverRows :: Num a => Eq a => Matrix a -> Integer -> Matrix a
iterateOverRows aMatrix r = aMatrix'
                    where c = selectNonZeroColumn aMatrix
                          rIndices = [1..(n aMatrix)]
                          rIndex = selectRowWithNonZeroColumn aMatrix rIndices c
                          aMatrix' = if r /= rIndex
                                     then rowInterchange r rIndex aMatrix
                                     else aMatrix

transformToRowEchelonForm :: Num a => Eq a => Matrix a -> Matrix a
transformToRowEchelonForm aMatrix = iterateOverRows aMatrix 1
