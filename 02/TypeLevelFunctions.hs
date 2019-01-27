{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE TypeFamilies   #-}


data Proxy a = Proxy

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True y = 'True
  Or 'False y = y

type family Not (x :: Bool) where
  Not 'True  = 'False
  Not 'False = 'True

type family Map (x :: a -> b) (i :: [a]) :: [b] where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

type family Foo (x :: Bool) (y :: Bool) :: Bool

type family Bar x y :: Bool -> Bool -> Bool

-- doesn't work
-- type family Baz :: Bool -> Bool -> Bool where
--  Baz = Or
