{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import StackVM

eval :: ExprT -> Integer
eval (ExprT.Lit n) = n
eval (ExprT.Add a b) = eval a + eval b
eval (ExprT.Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit n  = n > 0
    add = (||)
    mul = (&&)

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y)= MinMax (max x y) 
    mul (MinMax x) (MinMax y)= MinMax (min x y) 

instance Expr Mod7 where
    lit n = Mod7 (n `mod` 7)
    add (Mod7 x) (Mod7 y)= Mod7 ((x + y) `mod` 7)
    mul (Mod7 x) (Mod7 y)= Mod7 ((x * y) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5" 

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

instance Expr StackVM.Program where
    lit n = [PushI n]
    add a b = a ++ b ++ [StackVM.Add]
    mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

runVM :: String -> Either String StackVM.StackVal
runVM = maybe (Left "Parse Error") StackVM.stackVM . compile
