module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist list = reverse REmpty list
  where
    reverse revList [] = revList
    reverse revList (x:xs) = reverse (revList :< x) xs

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where
    showsPrec _ (REmpty) = showString ""
    showsPrec _ (REmpty :< x) = shows x
    showsPrec _ (xs :< x) =
        showsPrec 0 xs .
        showString "," .
        shows x

    show x = "[" ++ (showsPrec 0 x "") ++ "]"

instance Eq a => Eq (ReverseList a) where
    REmpty == REmpty = True
    (xs :< x) == (ys :< y) = x == y && xs == ys
    _ == _ = False
    
    x /= y = not (x == y)

instance Semigroup (ReverseList a) where
    ys <> REmpty = ys
    ys <> (xs :< x) = (ys <> xs) :< x

instance Monoid (ReverseList a) where
    mempty = REmpty

instance Functor ReverseList where
    fmap _ REmpty = REmpty
    fmap f (xs :< x) = fmap f xs :< f x

instance Applicative ReverseList where
    pure x = REmpty :< x
    REmpty <*> _ = REmpty
    _ <*> REmpty = REmpty
    fs :< f <*> xs = (fs <*> xs) <> fmap f xs

instance Monad ReverseList where
    REmpty >>= _ = REmpty
    xs :< x >>= f = (xs >>= f) <> f x 