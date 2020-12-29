---
title: "Notes (TLPH 07/15): Existential Types"
---

# 7.1 Existential Types and Eliminators

## Exercise 7.1 (i)

I think that the only functions of type `forall a. a -> r` are the constant
functions, since one of the things an `a` can be is `()` and the only thing we
can do from unit is return a constant. That is, we can't use any information
from `a` to determine `r`, because `a` might not give us *any* information at
all.

## Exercise 7.1 (ii)

Is this question just asking me to relay GHC's error message? If so:

```haskell
07/Existential.hs:16:36: error:
    • No instance for (Show t) arising from a use of ‘show’
      Possible fix:
        add (Show t) to the context of the data constructor ‘HasShow’
    • In the second argument of ‘(++)’, namely ‘show s’
      In the expression: "HasShow " ++ show s
      In an equation for ‘show’: show (HasShow s) = "HasShow " ++ show s
   |
16 |   show (HasShow s) = "HasShow " ++ show s
   |
```

see `07/Existential.hs`

## Exercise 7.1 (iii)

Since the only information `elimHasShow` has about the `a` in

```haskell
forall a. Show a => a -> r
```

is that `a` has a `Show` instance, the only functions `elimHasShow` can take are
either constant with respect to `a`, or use `a`'s `Show` instance.


## 7.1.1 Dynamic Types

It's cool how existential types, and a `Typeable` constraint to allow you to
recover the type turns out to be equivalent to dynamic typing. Yes, it's
unityping, as per the common saying "dynamic typing is just static typing with a
single type", but I think it's actually a little more interesting than that
here: We're bijecting the universe of Haskell types (constrained by `Typeable`)
into the `Dynamic` unitype. There no actual loss of information, we're just
turning static checking into runtime checking.

I'm not really a static typing ideologue; I think dynamic typing is an important
(though often abused) technique, because sometimes being able to selectively
turn off static checks gives you extraordinary power ("with great power comes
great responsibility").

---

Interestingly, the constraint synonym

```haskell
class (Monoid a, Eq a) => MonoidEq a
instance (Monoid a, Eq a) => MonoidEq a
```

required turning on `UndecidableInstances`. Not sure why.

## Scoping Information with Existentials

All the `seq`s in the typeclass instances are to make sure the `a` in `ST s a`
is strict.

---

This `ST` trick is such a hack. Like, it's clever to use `forall` scoping to
teach the type-system that we don't want to leak references, but the fact we
have to add an otherwise useless parameter `s` to `ST` just feels clunky.

I wonder how this trick relates to linear types?



