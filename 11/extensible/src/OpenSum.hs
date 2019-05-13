{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module OpenSum where

import           Data.Kind     (Type)
import           Data.Proxy
import           Fcf
import           GHC.TypeLits  hiding (type (+))
import           Unsafe.Coerce

data OpenSum (f :: k -> Type) (ts :: [k]) where
  UnsafeOpenSum :: Int -> f t -> OpenSum f ts

type FindElem (key :: k) (ts :: [k]) =
  FromMaybe Stuck =<< FindIndex (TyEq key) ts

type Member t ts = KnownNat (Eval (FindElem t ts))
type FriendlyMember f t ts = KnownNat (Eval (FriendlyFindElem f t ts))

type family FriendlyFindElem (f :: k -> Type) (t :: k) (ts :: [k]) where
  FriendlyFindElem f t ts = FromMaybe
    (TypeError
      ('Text "Attempted to call `friendlyPrj` to produce a `"
        ':<>: 'ShowType (f t)
        ':<>: 'Text "'."
        ':$$: 'Text "But the OpenSum can only contain one of:"
        ':$$: 'Text " "
        ':<>: 'ShowType ts
      )) =<< FindIndex (TyEq t) ts


findElem :: forall t ts. Member t ts => Int
findElem = fromIntegral . natVal $ Proxy @(Eval (FindElem t ts))

findFriendlyElem :: forall f t ts. FriendlyMember f t ts => Int
findFriendlyElem = fromIntegral . natVal $ Proxy @(Eval (FriendlyFindElem f t ts))

inj :: forall f t ts. Member t ts => f t -> OpenSum f ts
inj = UnsafeOpenSum (findElem @t @ts)

prj :: forall f t ts. Member t ts => OpenSum f ts -> Maybe (f t)
prj (UnsafeOpenSum i f) =
  if i == findElem @t @ts
  then Just $ unsafeCoerce f
  else Nothing

friendlyPrj :: forall f t ts. FriendlyMember f t ts => OpenSum f ts -> Maybe (f t)
friendlyPrj (UnsafeOpenSum i f) =
  if i == findFriendlyElem @f @t @ts
  then Just $ unsafeCoerce f
  else Nothing

decompose :: OpenSum f (t ': ts) -> Either (f t) (OpenSum f ts)
decompose = \case
  UnsafeOpenSum 0 t -> Left $ unsafeCoerce t
  UnsafeOpenSum n t -> Right $ UnsafeOpenSum (n - 1) t

weaken :: OpenSum f ts -> OpenSum f (x ': ts)
weaken (UnsafeOpenSum i f) = UnsafeOpenSum (i + 1) f

match :: forall f ts b . (forall t. f t -> b) -> OpenSum f ts -> b
match fn (UnsafeOpenSum _ t) = fn t


