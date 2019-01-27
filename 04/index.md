---
title: "Notes (TLPH 04/15): Working with Types
---

# 4.1 Type Scoping

see `04/Working.hs`

# 4.2 Type Applications

Type applications are applied in the order they appear in the signature. Another
way to think about it is that all type signatures with variables have an
implicit `forall`. So,

```haskell
(,) :: a -> b -> (a, b)
(,) :: forall a b. a -> b -> (a, b)
```

The variables in the implicit `forall` are in order of their appearance in the
rest of the type signature, but you can reorder the type variable with an
explicit `forall`.

Another fun little feature I discovered is `-XUnicodeSyntax` which let's you do

```haskell
(,) :: ∀ a b. a -> b -> (a, b)
```

with the unicode for-all `∀` symbol.

# 4.3 Ambiguous Types

Okay, so we can think of type signatures as functions from type variables to
concrete types:

```haskell
m :: ∀ a. Maybe a
m @Bool :: Maybe Bool
```

This has the kind of `Maybe :: Type -> Type` and `Bool :: Type`. We could
define:

```haskell
data Bool' = Bool
data Maybe' a = Maybe a
f = \ a -> Maybe a
```

Which would have `Maybe` as a data constructor and `Bool` as a data constant,
implying:

```
f Bool = Maybe Bool
```

The expressions `m` and `f` are substantially the same functions, but the former
is at the type level while the latter is at the value level.

However, the default behavior of this kind of type-level "function" differs from
those at the value level when it comes to handling constraints.

Suppose we have the function `show`:

```haskell

class Show a where
  show :: a -> String
```

Equivalently,

```haskell
show :: Show a => a -> String
```

The type variable `a` gets solved when `show'` is called, and if `a` has no
`Show` instance, then the compiler will error.

But if we have a similar function at the type level, `typeRep`

```haskell
typeRep :: ∀ a. Typeable a => TypeRep
```

Then the compiler will complain right away that the type variable `a` in the
constraint is not used in the right hand side of the signature

We can disable this, and defer the ambiguity check to the call site with
`-XAmbiguousTypes`.

Furthermore, ambiguious types can arise even when the type variable in the
constraint appears on the right-hand side of the signature:

```haskell
type family AlwaysUnit a where
  AlwaysUnit a = ()

g :: Show a => AlwaysUnit a -> String`
```
is ambiguous because `a` is phantom




