---
title: "Notes (TLPH 02/15): Terms, Types and Kinds"
---

# 2.1 The Kind System

A **kind** is a type of types. Compile-time is runtime for the compiler, and
values the compiler runs computation on are types.

## 2.1.1 The Kind of "Types"

Roughly, a "type" are type-level terms, a subset of which are "value
types", which can be inhabited by runtime values and correspond to the Haskell
kind `*` or `Type`. For example `Maybe` is a type, but not a `Type`, because it
has kind `Type -> Type`.


## 2.1.2 Arrow Kinds

Higher-kinded types are type constructors. `Maybe` is an HKT, because its kind
is `Type -> Type`.

## 2.1.3 Constraint Kinds

## Exercise 2.1.3 (i)

`Show` has kind `Type -> Constraint`

## Exercise 2.1.3 (ii)

`Functor` has kind `(Type -> Type) -> Constraint`, since it operates on unary
type constructors (like `[]`, `Maybe` etc.).

## Exercise 2.1.3 (iii)

`Monad` has kind `(Type -> Type) -> Constraint`

## Exercise 2.1.3 (iv)

The definition of `MonadTrans` is:

```haskell
class MonadTrans t where
  lift :: Monad m => m a -> t m a
```

From the class declaration, which must be a `Constraint`, we can see that
`MonadTrans` has kind `T -> Constraint`, where `T` is the kind of the `t` type
variable.

We can expand the kind of `T` by noticing that in the type signature for `lift`,
`t` is a type constructor that takes two arguments `m` and `a`, therefore
it's kind must be `M -> A -> Type`, where `M` is the kind of `m` and `A` is the
kind of `A`. Only `Type` kinds can appear in type signatures.

In the constraint on `lift`, the type variable `m` is an argument
to `Monad`, which has kind `(Type -> Type) -> Constraint`.

Therefore kind `M` is `Type -> Type`, and since `m a` has kind `Type`, the kind
of `a` is `Type`.

Therefore the kind of `t` is `(Type -> Type) -> Type -> Type`, and the kind of
`MonadTrans` is

```haskell
(Type -> Type) -> Type -> Type -> Constraint
```

# 2.2 Data Kinds

`-XDataKinds` allows lifting of data constructors into type constructors.

Seems like there are some bugs with the `UserType` example given in the text if
naively copied into a source file, but perhaps that wasn't the author's intent.
Regardless, was a simple fix:

```haskell
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

data UserType = UserK | AdminK

data Proxy (a :: UserType) = Proxy

data User = User { userAdminToken :: Maybe (Proxy 'AdminK) }

doSensitiveThings :: Proxy 'AdminK -> IO ()
doSensitiveThings = undefined
```

Which kind-errors on `doSensitveThings (a :: Proxy 'UserK)`


# 2.4 Type-Level Functions

For exercise solutions, see `02/TypeLevelFunctions.hs`




