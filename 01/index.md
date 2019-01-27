---
title: "Notes (TLPH 01/15): The Algebra Behind Types"
---

# 1.1 Isomorphisms and Cardinalities

An isomorphism between two types `a` and `b` is the pair of morphisms

```
f :: a -> b
g :: b -> a
```

such that

```
f . g = id :: b -> b
g . f = id :: a -> a
```

`a` isomorphic to `b` can be written `a ≅ b`.

The cardinality of a type is the number of inhabitants of that type.

Two types with the same finite cardinality `n` are isomorphic, and there are
`n!` unique isomorphisms between them.

# 1.2 Sum, Product and Exponential Types

cf. chapter 9 of CTFP.

## Exercise 1.2 (i)

```
Either Bool (Bool, Maybe Bool) -> Bool

|Bool| ^ (|Bool| + (|Bool| * |Maybe Bool|))

|Bool| = 2
|Maybe Bool| = 3

2 ^ (2 + 2 * 3) = 2^8
```

# 1.3 Tic-Tac-Toe

The example in the text would be much clearer if it were explained that the
function `(Three, Three) -> a` is the mapping from the boards grid coordinates
to the pieces on those coordinates. As in, we can either explicitly write down
every square on the board and its state, or we can assign a unique identifier
to each square and have a function from identifiers to states.

# 1.4 The Curry Howard Isomorphism

For exercises, see `01/Exercise.hs`.

# 1.5 Canonical Representations

The canonical representation of a type is as a sum of product types. That is,
the product types in the canonical representation are fully distributed over the
sum types.




