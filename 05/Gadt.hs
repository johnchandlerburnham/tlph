{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}

module Gadt where

five :: Int
five = 5

--five_ :: (a ~ Int) => a
--five_ = 5

data Expr a =
    LitInt Int
  | LitBool Bool
  | Add Int Int
  | Not Bool
  | If Bool (Expr a) (Expr a)

data Expr' a where
   LitInt'  :: Int -> Expr' Int
   LitBool' :: Bool -> Expr' Bool
   Add'     :: Expr' Int -> Expr' Int -> Expr' Int
   Not'     :: Expr' Bool -> Expr' Bool
   If'      :: Expr' Bool -> Expr' a -> Expr' a -> Expr' a

data Expr_ a =
    (a ~ Int) => LitInt_ Int
  | (a ~ Bool) => LitBool_ Bool
  | (a ~ Int) => Add_ (Expr_ Int) (Expr_ Int)
  | (a ~ Bool) => Not_ (Expr_ Bool)
  | If_ (Expr_ Bool) (Expr_ a) (Expr_ a)

evalExpr' :: Expr' a -> a
evalExpr' = \case
  LitInt' i  -> i
  LitBool' b -> b
  Add' x y   -> evalExpr' x + evalExpr' y
  Not' x     -> not $ evalExpr' x
  If' b x y  -> if evalExpr' b then evalExpr' x else evalExpr' y


