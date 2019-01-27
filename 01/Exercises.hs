-- 1.4 (i): Prove (a^b)^c = a^(b × c)
to :: (b -> c -> a) -> (b, c) -> a
to f (b,c) = f b c

from :: ((b, c) -> a) -> b -> c -> a
from f b c = f (b, c)

-- 1.4 (ii): Prove a^b × a^c = a^(b + c)
to' :: ((b -> a), (c -> a)) -> (Either b c -> a)
to' (f, g) (Left x)  = f x
to' (f, g) (Right x) = g x

from' :: (Either b c -> a) -> ((b -> a), (c -> a))
from' f = (f . Left, f . Right)

-- 1.4 (iii): Prove (a × b)^c = a^c × b^c
to'' :: (c -> (a, b)) -> (c -> a, c -> b)
to'' f = (fst . f, snd . f)

from'' :: (c -> a, c -> b) -> (c -> (a, b))
from'' (f, g) c = (f c, g c)
