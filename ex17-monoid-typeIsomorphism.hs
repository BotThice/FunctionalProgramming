-- give three more examples of monoids
-- the carrier sets must be different from the examples and from each other
-- be sure to reason about monoid laws
-- examples: 
-- 1) carrier: set of real numbers
--    operation: min
  --    identity law: identity of operation min is infinity.
    --    so, x <> infinity = x
    --    infinity <> x = x
  --   associativity law: (x <> y) <> z = x <> (y <> z)
    --    min(min(x, y), z) = min(x, min(y, z))
-- 2) carrier: set of subsets of set A
--    operation: Union
  --    identity law: identity of operation Union is empty set.
    --    so, x <> empty set = x
    --    empty set <> x = x
  --   associativity law: (x <> y) <> z = x <> (y <> z)
    --    (x U y) U z = x U (y U z)
-- 3) carrier: set of functions type a -> a
--    operation: (.)
  --    identity law: identity of operation (.) is id function.
    --    so, x <> id = x
    --    id <> x = x
  --   associativity law: (x <> y) <> z = x <> (y <> z)
    --    (f . g) . h = f . (g . h)


newtype Or = Or {getOr :: Bool} deriving (Eq, Ord, Read, Show, Bounded)
instance Semigroup Or where
  (Or x) <> (Or y) = Or (x || y)
instance Monoid Or where
  mempty = Or False

newtype And = And {getAnd :: Bool} deriving (Eq, Ord, Read, Show, Bounded)
instance Semigroup And where
  (And x) <> (And y) = And (x || y)
instance Monoid And where
  mempty = And True


maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b 
maybeBind Nothing _ = Nothing
maybeBind (Just x) f = f x

listBind :: [a] -> (a -> [b]) -> [b]
listBind ls f = concatMap f ls

eitherBind :: Either r a -> (a -> Either r b) -> Either r b
eitherBind (Left x) _ = Left x
eitherBind (Right x) f = f x

arrowBind :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
arrowBind g f = \x -> f (g x) x

pairBind :: (Monoid r) => (r, a) -> (a -> (r, b)) -> (r, b)
pairBind (r, x) f = f x



