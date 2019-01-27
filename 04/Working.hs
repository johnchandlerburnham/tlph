{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Working where

import           Data.Typeable

broken :: ∀ a b. (a -> b) -> a -> b
broken f a = apply
  where
    apply :: b
    apply = f a

foo :: TypeRep
foo = typeRep (Proxy :: Proxy Bool)

typeName :: ∀ a. Typeable a => String
typeName = show . typeRep $ Proxy @a

--m :: ∀ a. Maybe a
--m = undefined
--
---- > :t m @Bool :: Maybe Bool
--data Bool' = Bool deriving Show
--data Maybe' a = Maybe a deriving Show

--f = \ a -> Maybe a

--show' :: Show a => a -> String
--show' a = show a

type family AlwaysUnit a where
  AlwaysUnit a = ()


