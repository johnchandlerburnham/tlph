---
title: "Notes (TLPH 11/15): Extensible Data
---

# 11.2 Open Sums

## Exercise 11.2 (i)

So this seems pretty straighforward, all you need for `weaken` is to add
a component of the sum at the type level, but the value level remains the same.
Ergo, `unsafeCoerce`.

On second thought though, this will make the indices inconsistent, so maybe we
should do

```haskell
weaken :: OpenSum f ts -> OpenSum f (x ': ts)
weaken (UnsafeOpenSum i f) = UnsafeOpenSum (i + 1) f
```

instead?

# 11.3 Open Products

`Key` is like `Proxy` for kind `Symbol`.

## Exercise 11.3 (ii)

What's interesting here is that we're using two distinct mechanisms for deleting
an element: filtering by key at the type level, and direct index manipulation at
the value level.

The problem is that it seems like the consistency of the type level with the
value level isn't checked. If we delete at the type level, but not the value
level it looks like it still passses type checking just fine:

```haskell
delete :: forall key ts t f. KnownNat (FindElem key ts)
       => Key key
       -> OpenProduct f ts
       -> OpenProduct f (Eval (DeleteElem key ts))
delete _ (OpenProduct v) =
  let (a, b) = V.splitAt (findElem @key @ts) v
   in OpenProduct $ a V.++ b
```

## Exercise 11.3 (iii)

This was a difficult exercise, and I confess I looked at the solution in the
back of the book to figure out some of the mechanics, like using classes to
lower typelevel naturals and getting around the lack of type-level lambdas.

I think I mostly understand the techniques here, but I think I need some more
practice.

# 11.4 Overloaded Labels

Interestingly, `get #key result` gives a very long type error for me, whereas
`get (Key @"key") result` works fine.



