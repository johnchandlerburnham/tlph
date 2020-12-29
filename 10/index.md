---
title: "Notes (TLPH 10/15): First Class Families"
---

# 10.1 Defunctionalization

## Exercise 10.1-i

The fact that `Fst a b` is parameterized on both `a` and `b` confused me for a
moment. On this problem I wasn't sure what `ListMaybe` should be parameterized
on. Then I realized that what defunctionalization basically is taking a type
signature with an explicit `forall` and rewriting it:


```haskell
fst :: forall a b. (a, b) -> a
listMaybe :: forall a. [a] -> Maybe a
```

The transformation is a little like this:

```haskell
fst :: forall a b.      (a, b) -> a
Fst           a b = Fst (a, b)
```

And the return type goes into the functional dependency in the `Eval` class.

The really trippy thing is that we actually gain a degree of freedom here by
duplicating `Fst` as both a type and data constructor. Consider the pathological
case:

```haskell
data Fst' a b = Snd' (a, b)
data Snd' a b = Fst' (a, b)

instance Eval (Fst' a b) a where
  eval (Snd' (a, b)) = a

instance Eval (Snd' a b) b where
  eval (Fst' (a, b)) = b
```

And when we evaluate:
```
*FirstClass> eval (Fst' ("hello", True))
True
*FirstClass> eval (Snd' ("hello", True))
"hello"
```

## 10.2 Type-Level Defunctionalization

Oh cool! My observation from the previous chapter is answered here when we move
to the type-level, because type constructors automatically promote to kind
constructors, i.e. `Exp a` (which is just a wrapped `->`) is both a type and a
kind from a single declaration.

I think the `_1` in `type instance Eval (FromMaybe _1 ('Just a)) = a` is a
wildcard? It still compiles replacing `_1` with `_`. Standby...

Okay, having experimented, it's a *labeled* wildcard:

```haskell
type instance Eval (FromMaybe _1 ('Just a)) = _1
```

Which works as expected:

```haskell
*FirstClass> :kind! Eval (FromMaybe "nothing" ('Just "just"))
Eval (FromMaybe "nothing" ('Just "just")) :: GHC.Types.Symbol
= "nothing"
```

## Exercise 10.2-i

see `FirstClass.hs`


## Exercise 10.2-ii

see `FirstClass.hs`

```
*FirstClass> :kind! Eval (Foldr Cons '[] '[1, 2, 3, 4])
Eval (Foldr Cons '[] '[1, 2, 3, 4]) :: [GHC.Types.Nat]
= '[1, 2, 3, 4]
```

## 10.4 Ad-Hoc Polymorphism

## Exercise 10.4-i

```haskell
type instance Eval (Map f '(a, b)) = '(a, Eval (f b))
```
