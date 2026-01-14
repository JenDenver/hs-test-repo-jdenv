module Part6.Tests where

import qualified Data.Map

import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Part6.Tasks

unit_eye = do
    eye 1 @?= one
    eye 1 @?= [[one]]
    eye 1 @?= SparseMatrix 1 1 (Data.Map.fromList [((0, 0), one)])
    eye 2 @?= [[one, 0], [0, one]]
    eye 2 @?= SparseMatrix 2 2 (Data.Map.fromList [((0, 0), one), ((1, 1), one)])

    where one :: Int; one = 1

unit_zero = do
    zero 1 1 @?= zz
    zero 2 1 @?= [[zz, zz]]
    zero 2 2 @?= [[zz, zz], [zz, zz]]
    zero 5 5 @?= SparseMatrix 5 5 (Data.Map.fromList ([]::[((Int, Int), Int)]))
    where zz :: Int; zz = 0

unit_multiplyMatrix = do
    multiplyMatrix (2 :: Int) (3 :: Int) @?= 6

    let a = [[1, 2], [3, 4]] :: [[Int]]
    let b = [[5, 6], [7, 8]] :: [[Int]]
    multiplyMatrix a b @?= [[19, 22], [43, 50]]

    let c = [[1, 2, 3], [4, 5, 6]] :: [[Int]]
    let d = [[7, 8], [9, 10], [11, 12]] :: [[Int]]
    multiplyMatrix c d @?= [[58, 64], [139, 154]]

    multiplyMatrix a (eye 2 :: [[Int]]) @?= a
    multiplyMatrix (eye 2 :: [[Int]]) a @?= a

    multiplyMatrix a (zero 2 2 :: [[Int]]) @?= zero 2 2
    multiplyMatrix (zero 2 2 :: [[Int]]) a @?= zero 2 2

    let sparseA = setElement 0 0 1 $ setElement 0 1 2 $ setElement 1 0 3 $ setElement 1 1 4 $ empty 2 2 :: SparseMatrix Int
    let sparseB = setElement 0 0 5 $ setElement 0 1 6 $ setElement 1 0 7 $ setElement 1 1 8 $ empty 2 2 :: SparseMatrix Int
    let result = multiplyMatrix sparseA sparseB
    getElement 0 0 result @?= 19
    getElement 0 1 result @?= 22
    getElement 1 0 result @?= 43
    getElement 1 1 result @?= 50

    multiplyMatrix sparseA (eye 2 :: SparseMatrix Int) @?= sparseA

    let sparseMat = setElement 0 0 1 $ setElement 0 2 2 $ setElement 1 1 3 $ setElement 2 0 4 $ setElement 2 2 5 $ empty 3 3 :: SparseMatrix Int
    let sparseMat2 = setElement 0 0 1 $ setElement 1 1 2 $ setElement 2 2 3 $ empty 3 3 :: SparseMatrix Int
    let sparseResult = multiplyMatrix sparseMat sparseMat2
    getElement 0 0 sparseResult @?= 1
    getElement 1 1 sparseResult @?= 6
    getElement 2 2 sparseResult @?= 15


unit_determinant = do
    determinant (5 :: Int) @?= 5
    determinant (0 :: Int) @?= 0
    determinant ((-3) :: Int) @?= -3

    determinant ([[7]] :: [[Int]]) @?= 7

    determinant ([[1, 2], [3, 4]] :: [[Int]]) @?= -2
    determinant ([[5, 6], [7, 8]] :: [[Int]]) @?= -2

    determinant ([[2, 5, 3], [1, -2, -1], [1, 3, 4]] :: [[Int]]) @?= -20

    determinant (eye 1 :: [[Int]]) @?= 1
    determinant (eye 2 :: [[Int]]) @?= 1
    determinant (eye 3 :: [[Int]]) @?= 1

    determinant (zero 2 2 :: [[Int]]) @?= 0
    determinant (zero 3 3 :: [[Int]]) @?= 0

    let sparse2x2 = setElement 0 0 1 $ setElement 0 1 2 $ setElement 1 0 3 $ setElement 1 1 4 $ empty 2 2 :: SparseMatrix Int
    determinant sparse2x2 @?= -2

    let sparse3x3 = setElement 0 0 2 $ setElement 0 1 5 $ setElement 0 2 3 
                  $ setElement 1 0 1 $ setElement 1 1 (-2) $ setElement 1 2 (-1) 
                  $ setElement 2 0 1 $ setElement 2 1 3 $ setElement 2 2 4 
                  $ empty 3 3 :: SparseMatrix Int
    determinant sparse3x3 @?= -20

    determinant (eye 2 :: SparseMatrix Int) @?= 1

    determinant (zero 3 3 :: SparseMatrix Int) @?= 0

    let diagSparse = setElement 0 0 2 $ setElement 1 1 3 $ setElement 2 2 4 $ empty 3 3 :: SparseMatrix Int
    determinant diagSparse @?= 24
