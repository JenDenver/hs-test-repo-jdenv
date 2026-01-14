module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ result [] = result
myFoldl f result (x : xs) = myFoldl f (f result x) xs

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ result [] = result
myFoldr f result (x : xs) = f x (myFoldr f result xs)

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr (\x acc -> f x : acc) []

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = myFoldr (\x acc -> f x ++ acc) []

myConcat :: [[a]] -> [a]
myConcat = myFoldr (++) []

myReverse :: [a] -> [a]
myReverse = myFoldl (flip(:)) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldr (\x acc -> if p x then x : acc else acc) []

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = myFoldr select ([], [])
  where
    select x (trues, falses)
        | p x       = (x : trues, falses)
        | otherwise = (trues, x : falses)

