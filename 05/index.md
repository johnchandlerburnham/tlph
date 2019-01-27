---
title: "Notes (TLPH 05/15): Constraints and GADTS
---

# 5.1 Introduction

Type equalities let us impose additional constraints on type variables, by
telling the compiler to infer equivalence between types.

# 5.2 GADTs

see `05/GADT.hs`.

An Algebraic Data Type is canonically represented as a sum of tagged product types:

```haskell
data Expr a =
    LitInt Int
  | LitBool Bool
  | Add Int Int
  | Not Bool
  | If Bool (Expr a) (Expr a)
```

Each of these data constructors returns an `Expr a` type, which is not very
useful, since there's no mechanism in `Expr` to refine `a` into something more
concrete.

This is what the `-XGADTs` extension does.  Generalized Algebraic Data Types
allow ordinary Algebraic Data Types to return different refinements of `Expr a`
depending on its data constructor:

```haskell
data Expr' a where
   LitInt'  :: Int -> Expr' Int
   LitBool' :: Bool -> Expr' Bool
   Add'     :: Expr' Int -> Expr' Int -> Expr' Int
   Not'     :: Expr' Bool -> Expr' Bool
   If'      :: Expr' Bool -> Expr' a -> Expr' a -> Expr' a
```

These refinements are inferred through a type equality constraint, which the
GADT syntax is a syntactic sugar for (modulo some existential quantification):

```haskell
data Expr_ a =
    (a ~ Int) => LitInt_ Int
  | (a ~ Bool) => LitBool_ Bool
  | (a ~ Int) => Add_ (Expr_ Int) (Expr_ Int)
  | (a ~ Bool) => Not_ (Expr_ Bool)
  | If_ (Expr_ Bool) (Expr_ a) (Expr_ a)
```

# 5.3 Heterogeneous Lists

see `05/HList.hs`.

A heterogeneous list is a list that is polymorphic with respect to each element.
That is, an ordinary list is polymorphic over its elements, but all elements
must be of the same type:

```haskell
data List a = Cons a | Nil
```

Once the generic type variable `a` has been fixed by a single element, it is
fixed for all other elements:

```haskell
intList :: [Int]
intList = [3]

goodAppend = 2 : intList

badAppend = 'A' : intList -- TYPE ERROR
```


