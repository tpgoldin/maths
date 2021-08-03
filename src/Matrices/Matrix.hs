module Matrices.Matrix (
  Matrix(..),
  matrix,
  matrixAdd,
  matrixMult,
  scalarMultiplyBy,
  row,
  column,
  transpose,
  rowInterchange,
  rowScalarMultiply,
  rowLinearCombination,
  columnInterchange,
  columnScalarMultiply,
  columnLinearCombination
) where

import Data.Array

data Matrix a = Matrix { n::Integer, m::Integer, cells::Array (Integer, Integer) a } deriving Eq

instance Show a => Show (Matrix a) where
  show Matrix { n=r, m=c, cells=cs } = mconcat [show r, show "x", show c, show cs]

matrix :: Integer -> Integer -> [((Integer, Integer), a)] -> Matrix a
matrix r c xs = Matrix { n=r, m=c, cells=array ((1, 1), (r, c)) xs }

selectZeroOrOne :: Num a => Integer -> Integer -> a
selectZeroOrOne i j = if i == j
                      then 1
                      else 0
                        
zeroMatrix :: Num a => Integer -> Integer -> Matrix a
zeroMatrix r c = matrix r c xs
                 where xs = [((i, j), 0) | i <- [1..r], j <- [1..c]]

idMatrix :: Num a => Integer -> Integer -> Matrix a
idMatrix r c = matrix r c xs
                 where xs = [((i, j), selectZeroOrOne i j) | i <- [1..r], j <- [1..c]]

matrixAdd :: Num a => Matrix a -> Matrix a -> Maybe (Matrix a)
matrixAdd Matrix { n=r1, m=c1, cells=cs1 } Matrix { n=r2, m=c2, cells=cs2 } = if r1 /= r2 || c1 /= c2
                                                                              then Nothing
                                                                              else Just (Matrix { n=r1, m=c1, cells=array ((1, 1), (r1, c1)) cs'})
                                                                              where rng = indices cs1
                                                                                    cs' = [(idx, (cs1 ! idx) + (cs2 ! idx)) | idx <- rng]

scalarMultiplyBy :: Num a => Matrix a -> a -> Matrix a
scalarMultiplyBy Matrix { n=r, m=c, cells=cs } w = matrix r c cs'
                                                      where rng = indices cs
                                                            cs' = [(idx, w * (cs ! idx)) | idx <- rng]

transpose :: Matrix a -> Matrix a
transpose Matrix { n=r, m=c, cells=cs } = Matrix { n=c, m=r, cells=array ((1, 1), (c, r)) cs'}
                                          where rng = indices cs
                                                cs' = [((snd idx, fst idx), cs ! idx) | idx <- rng]

row :: Matrix a -> Integer -> [a]
row x i = r
          where cs = cells x
                rng = indices cs
                r = [cs ! e | e <- rng, fst e == i]

column :: Matrix a -> Integer -> [a]
column x j = c
          where cs = cells x
                rng = indices cs
                c = [cs ! e | e <- rng, snd e == j]

innerProduct :: Num a => [a] -> [a] -> a
innerProduct (x:xs) (y:ys) = (x * y) + innerProduct xs ys
innerProduct _ _ = 0

matrixMult :: Num a => Matrix a -> Matrix a -> Maybe (Matrix a)
matrixMult m1 m2 = if c1 /= r2
                   then Nothing
                   else Just m'
                   where r1 = n m1
                         c1 = m m1
                         r2 = n m2
                         c2 = m m2
                         m' = Matrix { n=r1, m=c2, cells=array ((1, 1), (r1, c2)) cs' }
                         cs' = [((i, j), (m1 `row` i) `innerProduct` (m2 `column` j)) | i <- [1..r1], j <- [1..c2]]

interchangeIndices :: Integer -> Integer -> Integer -> Integer
interchangeIndices x i1 i2
                         | x == i1 = i2
                         | x == i2 = i1
                         | otherwise = x

rowInterchange :: Integer -> Integer -> Matrix a -> Matrix a
rowInterchange r1 r2 aMatrix = Matrix { n=r, m=c, cells=array ((1, 1), (r, c)) cs' }
                         where r = n aMatrix
                               c = m aMatrix
                               cs = cells aMatrix
                               rng = indices cs
                               cs' = [((interchangeIndices (fst e) r1 r2, snd e), cs ! e) | e <- rng]

rScalarMultiply :: Num a => (Integer, Integer) -> Integer -> a -> a -> a
rScalarMultiply e r0 sf x = if fst e == r0
                            then x * sf
                            else x

rowScalarMultiply :: Num a => Integer -> a -> Matrix a -> Matrix a
rowScalarMultiply r0 sf aMatrix = Matrix { n=r, m=c, cells=array ((1, 1), (r, c)) cs' }
                                  where r = n aMatrix
                                        c = m aMatrix
                                        cs = cells aMatrix
                                        rng = indices cs
                                        cs' = [ (e, rScalarMultiply e r0 sf (cs ! e)) | e <- rng]


selectColumnValue :: Num a => (Integer, Integer) -> Integer -> [a] -> a -> a
selectColumnValue e r1 zs x = if fst e == r1
                    then zs !! fromIntegral (snd e - 1)
                    else x

selectRowValue :: Num a => (Integer, Integer) -> Integer -> [a] -> a -> a
selectRowValue e c1 zs x = if snd e == c1
                    then zs !! fromIntegral (fst e - 1)
                    else x

rowLinearCombination :: Num a => Integer -> a -> Integer -> Matrix a -> Matrix a
rowLinearCombination r0 sf r1 aMatrix = Matrix { n=r, m=c, cells=array ((1, 1), (r, c)) cs' }
                                        where r = n aMatrix
                                              c = m aMatrix
                                              cs = cells aMatrix
                                              xs = map (* sf) (aMatrix `row` r0)
                                              ys = aMatrix `row` r1
                                              zs = zipWith (+) xs ys
                                              rng = indices cs
                                              cs' = [ (e, selectColumnValue e r1 zs (cs ! e)) | e <- rng]

columnInterchange :: Integer -> Integer -> Matrix a -> Matrix a
columnInterchange c1 c2 aMatrix = Matrix { n=r, m=c, cells=array ((1, 1), (r, c)) cs' }
                         where r = n aMatrix
                               c = m aMatrix
                               cs = cells aMatrix
                               rng = indices cs
                               cs' = [((fst e, interchangeIndices (snd e) c1 c2), cs ! e) | e <- rng]

cScalarMultiply :: Num a => (Integer, Integer) -> Integer -> a -> a -> a
cScalarMultiply e c0 sf x = if snd e == c0
                            then x * sf
                            else x

columnScalarMultiply :: Num a => Integer -> a -> Matrix a -> Matrix a
columnScalarMultiply c0 sf aMatrix = Matrix { n=r, m=c, cells=array ((1, 1), (r, c)) cs' }
                                        where r = n aMatrix
                                              c = m aMatrix
                                              cs = cells aMatrix
                                              rng = indices cs
                                              cs' = [ (e, cScalarMultiply e c0 sf (cs ! e)) | e <- rng]

columnLinearCombination :: Num a => Integer -> a -> Integer -> Matrix a -> Matrix a
columnLinearCombination c0 sf c1 aMatrix = Matrix { n=r, m=c, cells=array ((1, 1), (r, c)) cs' }
                                           where r = n aMatrix
                                                 c = m aMatrix
                                                 cs = cells aMatrix
                                                 xs = map (* sf) (aMatrix `column` c0)
                                                 ys = aMatrix `column` c1
                                                 zs = zipWith (+) xs ys
                                                 rng = indices cs
                                                 cs' = [ (e, selectRowValue e c1 zs (cs ! e)) | e <- rng]