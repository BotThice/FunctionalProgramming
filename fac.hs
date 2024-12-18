fac :: (Eq t, Num t) => t -> t
fac 0 = 1
fac n = n * fac (n - 1)

-- type of fac is fac :: (Eq t, Num t) => t -> t
-- this version of code is not secure becuase of when n is fraction it will not reach case n=0 so it will not stop compute

