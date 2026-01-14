{-# LANGUAGE FlexibleInstances #-}

module Part6.Tasks where

import Util (notImplementedYet)
import qualified Data.Map as Map
import Data.Map (Map)

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix m where
       getDimensions :: m -> (Int, Int)
       getElement :: Int -> Int -> m -> Int
       setElement :: Int -> Int -> Int -> m -> m
       empty :: Int -> Int -> m

       height :: m -> Int
       height m = fst (getDimensions m)
    
       width :: m -> Int
       width m = snd (getDimensions m)

       arbitaryM :: Int -> Int -> (Int -> Int -> Int) -> m
       arbitaryM w h f = buildMatrix 0 0 (empty h w)
        where
         buildMatrix row col result
            | row >= h   = result
            | col >= w   = buildMatrix (row + 1) 0 result
            | otherwise  = buildMatrix row (col + 1) (setElement row col (f col row) result)
    
       subMatrix :: m -> Int -> Int -> m
       subMatrix matrix rowToRemove colToRemove = buildMatrix 0 0 (empty (height matrix - 1) (width matrix - 1))
        where
         buildMatrix i j result
            | i >= height matrix - 1 = result
            | j >= width matrix - 1  = buildMatrix (i + 1) 0 result
            | otherwise              = buildMatrix i (j + 1) (setElement i j value result)
          where
            origRow = if i >= rowToRemove then i + 1 else i
            origCol = if j >= colToRemove then j + 1 else j
            value = getElement origRow origCol matrix

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
       getDimensions _ = (1, 1)
    
       getElement row col m
        | row == 0 && col == 0 = m
        | otherwise = 0
    
       setElement row col value m
        | row == 0 && col == 0 = value
        | otherwise = m
    
       empty height width
        | height == 1 && width == 1 = 0
        | otherwise = 0

instance Matrix [[Int]] where
       getDimensions [] = (0, 0)
       getDimensions (row:rows) = (1 + length rows, length row)
    
       getElement row col matrix
         | row < 0 || col < 0 = 0
         | row >= length matrix = 0
         | col >= length (matrix !! row) = 0
         | otherwise = (matrix !! row) !! col
    
       setElement row col value matrix
         | row < 0 || col < 0 = matrix
         | row >= h || col >= w = matrix
         | otherwise = take row matrix 
              ++ [take col (matrix !! row) ++ [value] ++ drop (col + 1) (matrix !! row)]
              ++ drop (row + 1) matrix
         where
             (h, w) = getDimensions matrix
    
       empty height width = replicate height (replicate width 0)

instance Matrix (SparseMatrix Int) where
       getDimensions m = (sparseMatrixHeight m, sparseMatrixWidth m)
    
       getElement row col m
         | row < 0 || col < 0 = 0
         | row >= sparseMatrixHeight m = 0
         | col >= sparseMatrixWidth m = 0
         | otherwise = Map.findWithDefault 0 (row, col) $ sparseMatrixElements m
    
       setElement row col value m
         | row < 0 || col < 0 = m
         | row >= h || col >= w = m
         | otherwise = m { sparseMatrixElements = Map.insert (row, col) value $ sparseMatrixElements m }
         where
              (h, w) = getDimensions m
    
       empty height width = SparseMatrix {
              sparseMatrixWidth = width,
              sparseMatrixHeight = height,
              sparseMatrixElements = Map.empty}

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye n = go 0 (empty n n)
  where
       go i m
         | i >= n    = m
         | otherwise = go (i + 1) (setElement i i 1 m)

-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero w h = empty h w

-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix l r
    | width l /= height r = error "Invalid matrix size"
    | otherwise = arbitaryM (width r) (height l) (\x y -> sum [getElement y k l * getElement k x r | k <- [0 .. width l - 1]])

-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant m
    | width m /= height m = error "Invalid matrix size"
    | width m == 0        = 1
    | width m == 1        = getElement 0 0 m
    | otherwise           = sum [(-1) ^ col * getElement 0 col m * determinant (subMatrix m 0 col) | col <- [0 .. width m - 1]]
