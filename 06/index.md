---
title: "Notes (TLPH 06/15): Rank-N Types
---

# 6.1 Introduction

```haskell
applyToFive :: (a -> a) -> Int
applyToFive  f = f 5
```

With explicit quantification:

```haskell
applyToFive :: forall a. (a -> a) -> Int
applyToFive  f = f 5

id :: forall a. a -> a
id a = a
```

The distinction between the above two type signatures can be clarified by:

```haskell
> type Endo a = a -> a
> type Id a = forall. a -> a
> :t not :: Endo Int
> :t id :: Id a
```

In other words:

```haskell
applyToFive :: forall a. Endo a -> Int
applyToFive  f = f 5
```

which simply won't work. `f` can't be *any* endomorphism to `5`, but only an
`Endo Int` (because of the inference from the return type `Int`).

If we want to only accept the identity, though:

```haskell
idFive :: Id a -> Int
idFive  f = f 5
```

which is just:

```
idFive :: (forall a. a -> a) -> Int
idFive  f = f 5
```

# 6.3 The Nitty Gritty Details

## Exercise 6.3 (i)

```haskell
Int -> forall a. a -> a
```

is rank-1, because `Id a = (forall a. a ~ a)` is rank-1.

## Exercise 6.3 (ii)

```haskell
(a -> b) -> (forall c. c -> a) -> b
```

is rank-2.

## Exercise 6.3 (iii)


```haskell
((forall x. m x -> b (z m x)) -> b (z m a)) -> m a
```

is rank-3.

## 6.4 The Continuation Monad

The first function

```haskell
cont :: a -> (forall r. (a -> r) -> r)
cont a = \f -> f a
```

says that "given an `a`, then all functions `f` that accept an `a` and
produces any type `r`, will produce that `r` if called with `a`.

The second function

```haskell
runCont :: (forall r. (a -> r) -> r) -> a
runCont f = let callback = id in f callback
```

is saying that "if you have a function `f` that can take as input all functions
from type `a` to any other type `r`, then you can produce a type `a` by passing
`f` the function `a ~ r => a -> r`, i.e. the identity function, which is of type
`(a -> r)` when `a` and `r` are the same type.

## Exercise 6.4 (i)

I can cheat with category theory here:

```haskell
newtype Identity a = Identity { unIdentity :: a }

--                f' 
--     Identity a -> Identity b
--     ^ |              ^ |
--   g | | g'         h | | h'
--     | v              | v
--     Cont a     ->   Cont b
--             (fmap f)

f' :: Identity a -> Identity b
f' = fmap f

g :: Cont a -> Identity a
g = Identity . runCont

g' :: Identity a -> Cont a
g' = cont . unIdentity
```

Notice that the above polymorphic `g` and `g'` are the same as `h` and `h'`
respectively. Therefore

```haskell
instance Functor Cont where
  fmap' :: (a -> b) -> Cont a -> Cont b
  fmap f = g' . f' . g
```

Or more simply:

```haskell
instance Functor Cont where
  fmap f = cont . f . runCont
```

Okay, maybe that wasn't cheating. Was still fun though

(Note that this requires slightly modifying `cont` and `runCont`. See
`06/Cont.hs`)

## Exercise 6.4 (ii)

See `06/Cont.hs`

## Exercise 6.4 (iii)

See `06/Cont.hs`

## Exercise 6.4 (iv)

See `06/Cont.hs`
