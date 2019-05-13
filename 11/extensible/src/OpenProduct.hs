{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module OpenProduct where

import           Data.Kind            (Constraint, Type)
import           Data.Proxy           (Proxy (..))
import qualified Data.Vector          as V
import           Fcf
import           GHC.OverloadedLabels (IsLabel (..))
import           GHC.TypeLits         hiding (type (+))
import           Unsafe.Coerce        (unsafeCoerce)

data Any (f :: k -> Type) where
  Any :: f t -> Any f

data OpenProduct (f :: k -> Type) (ts :: [(Symbol, k)]) where
  OpenProduct :: V.Vector (Any f) -> OpenProduct f ts

nil :: OpenProduct f '[]
nil = OpenProduct V.empty

data Key (key :: Symbol) = Key

insert' :: Key key -> f t -> OpenProduct f ts -> OpenProduct f ('(key, t) ': ts)
insert' _ ft (OpenProduct v) = OpenProduct $ V.cons (Any ft) v

type UniqueKey (key :: k) (ts :: [(k, t)]) =
  Null =<< Filter (TyEq key <=< Fst) ts

type family RequireUniqueKey
  (result :: Bool) (key :: Symbol) (t :: k) (ts :: [(Symbol, k)]) :: Constraint where
  RequireUniqueKey 'True key t ts = ()
  RequireUniqueKey 'False key t ts =
    TypeError ( 'Text "Attempting to add a field named `"
          ':<>: 'Text key
          ':<>: 'Text "' with type "
          ':<>: 'ShowType t
          ':<>: 'Text " to an OpenProduct."
          ':$$: 'Text "But the OpenProduct already has a field `"
          ':<>: 'Text key
          ':<>: 'Text "' with type "
          ':<>: 'ShowType (LookupType key ts)
          ':$$: 'Text "Consider using `update' "
          ':<>: 'Text "instead of `insert'."
              )

friendlyInsert :: RequireUniqueKey (Eval (UniqueKey key ts)) key t ts
               => Key key
               -> f t
               -> OpenProduct f ts
               -> OpenProduct f ('(key, t) ': ts)
friendlyInsert _ ft (OpenProduct v) = OpenProduct $ V.cons (Any ft) v

insert :: Eval (UniqueKey key ts) ~ 'True
       => Key key
       -> f t
       -> OpenProduct f ts
       -> OpenProduct f ('(key, t) ': ts)
insert _ ft (OpenProduct v) =  OpenProduct $ V.cons (Any ft) v

type FindElem (key :: Symbol) (ts :: [(Symbol, k)]) =
  Eval (FromMaybe Stuck =<< FindIndex (TyEq key <=< Fst) ts)

findElem :: forall key ts. KnownNat (FindElem key ts) => Int
findElem = fromIntegral . natVal $ Proxy @(FindElem key ts)

type LookupType (key :: k) (ts :: [(k, t)]) =
  FromMaybe Stuck =<< Lookup key ts

get :: forall key ts f. KnownNat (FindElem key ts)
    => Key key
    -> OpenProduct f ts
    -> f (Eval (LookupType key ts))
get _ (OpenProduct v) = unAny $ V.unsafeIndex v $ findElem @key @ts
  where
    unAny (Any a) = unsafeCoerce a

type UpdateElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)]) =
  SetIndex (FindElem key ts) '(key, t) ts

update :: forall key ts t f. KnownNat (FindElem key ts)
       => Key key
       -> f t
       -> OpenProduct f ts
       -> OpenProduct f (Eval (UpdateElem key t ts))
update _ ft (OpenProduct v) =
  OpenProduct $ v V.// [(findElem @key @ts, Any ft)]


type DeleteElem (key :: Symbol) (ts :: [(Symbol, k)]) =
  Filter (Not <=< TyEq key <=< Fst) ts

delete :: forall key ts t f. KnownNat (FindElem key ts)
       => Key key
       -> OpenProduct f ts
       -> OpenProduct f (Eval (DeleteElem key ts))
delete _ (OpenProduct v) =
  let (a, b) = V.splitAt (findElem @key @ts) v
   in OpenProduct $ a V.++ (V.tail b)

type family FindKeyIndex (key :: Symbol) (ts :: [(Symbol, k)]) :: (Maybe Nat) where
  FindKeyIndex key '[] = 'Nothing
  FindKeyIndex key ('(key',t) ': ts) =
    If (Eval (TyEq key key')) ('Just 0)
       (Eval (Map ((+) 1) =<< Pure (FindKeyIndex key ts)))

type UpsertElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)]) =
  FromMaybe ('(key, t) ': ts)
    =<< Map (SetIndex' '(key, t) ts) (FindKeyIndex key ts)

data SetIndex' :: k -> [k] -> Nat -> Exp [k]
type instance Eval (SetIndex' a as n) = Eval (SetIndex n a as)

class FindUpsertElem (a :: Maybe Nat) where
  upsertElem :: Maybe Int

instance FindUpsertElem 'Nothing where
  upsertElem = Nothing

instance KnownNat n => FindUpsertElem ('Just n) where
  upsertElem = Just. fromIntegral . natVal $ Proxy @n

upsert :: forall key ts t f. FindUpsertElem (FindKeyIndex key ts)
       => Key key
       -> f t
       -> OpenProduct f ts
       -> OpenProduct f (Eval (UpdateElem key t ts))
upsert k ft (OpenProduct v) = OpenProduct $ case upsertElem @(FindKeyIndex key ts) of
  Nothing -> V.cons (Any ft) v
  Just n  -> v V.// [(n, Any ft)]
