module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) left right = BinaryTerm Plus left right
infixl 6 |+|
(|-|) :: Term -> Term -> Term
(|-|) left right = BinaryTerm Minus left right
infixl 6 |-|
(|*|) :: Term -> Term -> Term
(|*|) left right = BinaryTerm Times left right
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression
   | IntConstant n <- expression = IntConstant n
   | Variable oldVar <- expression, oldVar == varName = replacement
   | Variable anyVar <- expression = Variable anyVar
   | BinaryTerm op left right <- expression = BinaryTerm op 
         (replaceVar varName replacement left) 
         (replaceVar varName replacement right)

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate exp
   | Variable x <- exp = Variable x
   | IntConstant n <- exp = IntConstant n
   | BinaryTerm op left right <- exp =
      let evalLeft = evaluate left
          evalRight = evaluate right
      in case (evalLeft, evalRight) of
         (IntConstant n1, IntConstant n2) ->
            case op of
               Plus -> IntConstant (n1 + n2)
               Minus -> IntConstant (n1 - n2)
               Times -> IntConstant (n1 * n2)
         _ -> BinaryTerm op (evalLeft) (evalRight)