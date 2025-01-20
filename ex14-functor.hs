-- suppose we want to define a data type that counts the number of transformations done to the data:
data COp a = CVal Int a
    deriving (Show)

-- is this an appropriate definition of a functor for COp?
instance Functor COp where
  fmap f (CVal c v)
      = CVal (c+1) (f v)
-- this definition is not appropriate of the functor for COp 
-- because when f is id it should get identity of all value in COp
-- vut this definition will give c+1 instead of id(c)
-- so, it break rule(1) of functor law
-- example: x = CVal 0 "Hello World"
-- fmap id x will get CVal 1 "Hello World" instead of CVal 0 "Hello World"


-- define a functor for (->) r:
instance Functor ((->) r) where
    fmap f g = \r -> f (g r)
