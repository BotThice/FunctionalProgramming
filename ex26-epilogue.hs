s :: (a -> b -> c) -> (a -> b) -> a -> c
s x y z = x z (y z)
k :: a -> b -> a
k x y = x
i :: a -> a
i x = x

(.) :: (b -> c) -> (a -> b) -> (a -> c)
-- (.) g f = g (f x)
-- (.) = \g f x -> s (k s) k g f x
(.) = s (k s) k
-- ((k s) g (k g)) f x
-- ((s (k g)) f) x
-- (s (k g) f) x
-- ((k g) x) (f x)
-- g (f x)

(&) :: a -> (a -> b) -> b
(&) x f = s (s (k i) k) (k i) f x

flip :: (a -> b -> c) -> b -> a -> c
flip f b a = s (k) (k i) f a b
