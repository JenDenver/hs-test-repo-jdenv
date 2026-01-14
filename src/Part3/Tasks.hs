module Part3.Tasks where

import Data.Char (digitToInt)
import Data.List (group, sort, sortBy, groupBy)
import Data.Ord (comparing)
import Util (notImplementedYet)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = fmap f [n, n+1..]

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq list = fst $ last $ sortBy (comparing snd) $ count list
    where
        count :: [Int] -> [(Int, Int)]
        count digits = map (\l -> (head l, length l)) $ group $ sort $ allDigits digits

        allDigits :: [Int] -> [Int]
        allDigits = concatMap extractDigits

        extractDigits :: Int -> [Int]
        extractDigits n = map digitToInt $ show $ abs n

    -- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (x:xs) = x : uniq (filter (/= x) xs)

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f lst = [(key, [x | (k, x) <- pairs, k == key]) | key <- keys]
  where
    keys = removeDuplicates (map fst pairs)
    removeDuplicates [] = []
    removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)
    pairs = [(f x, x) | x <- lst]
    
    
    
    
   