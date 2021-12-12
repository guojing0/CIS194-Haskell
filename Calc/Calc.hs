{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Calc where

import qualified ExprT as E
import Parser
import qualified StackVM as S

import qualified Data.Map as M
import Control.Applicative

eval :: E.ExprT -> Integer
eval (E.Lit n) = n
eval (E.Add a b) = eval a + eval b
eval (E.Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp E.Lit E.Add E.Mul

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr E.ExprT where
    lit = E.Lit
    add = E.Add
    mul = E.Mul

reify :: E.ExprT -> E.ExprT
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

instance Expr S.Program where
    lit n = [S.PushI n]
    add a b = a ++ b ++ [S.Add]
    mul a b = a ++ b ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul

runVM :: String -> Either String S.StackVal
runVM = maybe (Left "Parse Error") S.stackVM . compile

class HasVars a where
    var :: String -> a

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
    lit = Lit
    add = Add
    mul = Mul

instance HasVars VarExprT where
    var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit a = const $ Just  a
    add a b = liftA2 (+) <$> a <*> b
    mul a b = liftA2 (*) <$> a <*> b

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
