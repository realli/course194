{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import qualified StackVM as S
import qualified Data.Map as M
import Control.Applicative ( (<*>), (<$>) )

-- some new types
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

-- a class for abstraction of ExprT
class Expr a where
    mul :: a -> a -> a
    add :: a -> a -> a
    lit :: Integer -> a

-- class for store variables
class HasVars a where
    var :: String -> a

-- make ExprT instance of Expr
instance Expr ExprT where
    mul = Mul
    add = Add
    lit = Lit

-- Integer is an instance of Expr, works like calculator
instance Expr Integer where
    mul = (*)
    add = (+)
    lit = id

-- new instance for MinMax
instance Expr MinMax where
    lit = MinMax
    mul (MinMax a) (MinMax b) = MinMax (max a b)
    add (MinMax a) (MinMax b) = MinMax (min a b)

-- new instance for Mod7
instance Expr Mod7 where
    lit = Mod7 . (`mod` 7)
    mul (Mod7 a) (Mod7 b) = lit $ a * b
    add (Mod7 a) (Mod7 b) = lit $ a + b

-- Bool is an instance of Expr, value > 0 are treated as True
-- <= 0 are treated as False
-- and mul works like logic 'and'
-- add works like logic 'or'
instance Expr Bool where
    lit = (>0)
    add = (||)
    mul = (&&)

-- eval an expr
eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add exp1 exp2) = eval exp1 + eval exp2
eval (Mul exp1 exp2) = eval exp1 * eval exp2

-- eval String to value(Maybe)
evalStr :: String -> Maybe Integer
evalStr str = do
    exprt <- parseExp Lit Add Mul str
    return (eval exprt)


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

-- make Program an instance of Expr
instance Expr S.Program where
    lit x = [S.PushI x]
    mul a b = a ++ b ++ [S.Mul]
    add a b = a ++ b ++ [S.Add]

-- a new type which can store variable
data VarExprT = VarLit Integer
           | Var String
           | VarAdd VarExprT VarExprT
           | VarMul VarExprT VarExprT
  deriving (Show, Eq)

-- make instance of VarExprT
instance Expr VarExprT where
    lit = VarLit
    mul = VarMul
    add = VarAdd

instance HasVars VarExprT where
    var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup
instance Expr (M.Map String Integer -> Maybe Integer) where
    lit = flip $ const Just
    mul f1 f2 = \m -> do
                    left <- f1 m
                    right <- f2 m
                    return (left * right)
    add f1 f2 = \m -> (+) <$> f1 m <*> f2 m


withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

