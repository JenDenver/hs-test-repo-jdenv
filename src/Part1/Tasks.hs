module Part1.Tasks where

import Data.Fixed (mod')
import Data.List (sort, reverse)
import Util(notImplementedYet)

factorial :: Integer -> Integer
factorial n = product [1..n]

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = let
            normalize x
              | x' > pi   = x' - 2 * pi
              | otherwise = x'
              where x' = x `mod'` (2 * pi)
              
            maclaurin x n = sum (fmap rowterm [0..n])
              where
                  rowterm n = (((-1) ^ n) * (x ^ (2 * n + 1))) / fromIntegral (factorial (2 * n + 1))
                  
        in maclaurin (normalize x) 7

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = let
            normalize x
              | x' > pi   = x' - 2 * pi
              | otherwise = x'
              where x' = x `mod'` (2 * pi)
              
            maclaurin x n = sum (fmap rowterm [0..n])
              where
                  rowterm n = (-1) ^^ n * x ^^ (2 * n) / fromIntegral (factorial (2 * n))
                  
          in maclaurin (normalize x) 7

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD x y
  | y == 0 = abs x
  | otherwise = myGCD y $ mod x y

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year
  | year <= 0                           = False
  | month < 1 || month > 12             = False
  | day < 1 || day > daysNum month year = False
  | otherwise                           = True
    where
      isLeapYear :: Integer -> Bool
      isLeapYear year
        | year `mod` 400 == 0 = True
        | year `mod` 100 == 0 = False
        | year `mod` 4 == 0   = True
        | otherwise           = False
    
      daysNum :: Integer -> Integer -> Integer
      daysNum month year
        | month `elem` [1, 3, 5, 7, 8, 10, 12]  = 31
        | month `elem` [4, 6, 9, 11]            = 30
        | month == 2 && isLeapYear year         = 29
        | month == 2                            = 28
        | otherwise                             = 0


-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow x n
  | n == 0 = 1
  | n == 1 = x
  | n > 1 = x * myPow x (n-1)


-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x
  | x < 2           = False
  | x == 2          = True
  | x `mod` 2 == 0  = False
  | otherwise       = not (any isDivisor [3, 5 .. ceiling . sqrt $ fromInteger x])
    where
      isDivisor :: Integer -> Bool
      isDivisor n = x `mod` n == 0


type Point2D = (Double, Double)

  -- рассчитайте площадь многоугольника по формуле Гаусса
  -- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points
  | length points < 3 = 0
  | otherwise = abs total / 2
  where
    closedPoints = points ++ [head points]
    crossStep :: Point2D -> Point2D -> Double
    crossStep (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

    total = sum $ zipWith crossStep points (tail closedPoints)
    


-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
  | not (isTriangle a b c) = -1
  | c'^2 > a'^2 + b'^2 = 0
  | c'^2 < a'^2 + b'^2 = 1
  | c'^2 == a'^2 + b'^2 = 2
    where
      [a', b', c'] = sort [a, b, c]
      isTriangle :: Double -> Double -> Double -> Bool
      isTriangle a b c
        | a < 0 || b < 0 || c < 0 = False
        | c' >= a'+ b' = False
        | otherwise = True
