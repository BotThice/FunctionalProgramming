class IfValue a where
  boolVal :: a -> Bool

instance IfValue Integer where
    boolVal 0 = False
    boolVal _ = True

instance IfValue Int where
    boolVal 0 = False
    boolVal _ = True

instance IfValue Float where
    boolVal 0 = False
    boolVal _ = True

instance IfValue Double where
    boolVal 0 = False
    boolVal _ = True

instance IfValue Char where
    boolVal '\0' = False
    boolVal _ = True

instance IfValue String where
    boolVal "" = False
    boolVal _ = True

instance IfValue Bool where
    boolVal False = False
    boolVal _ = True

instance IfValue [a] where
    boolVal [] = False
    boolVal _ = True

instance IfValue (Maybe a) where
    boolVal Nothing = False
    boolVal _ = True

data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show)
instance IfValue (Tree a) where
    boolVal Empty = False
    boolVal _ = True


mapMaybe :: (a -> b) -> [Maybe a] -> [Maybe b]
mapMaybe _ [] = []
mapMaybe f (x:xs) = case x of
    Nothing -> Nothing : mapMaybe f xs
    Just x -> Just (f x) : mapMaybe f xs

mapPair :: (a -> c) -> (b -> d) -> [(a, b)] -> [(c, d)]
mapPair _ _ [] = []
mapPair f g ((a, b):xs) = (f a, g b) : mapPair f g xs

mapPair2 :: ((a, b) -> (c, d)) -> [(a, b)] -> [(c, d)]
mapPair2 _ [] = []
mapPair2 f (x:xs) = f x : mapPair2 f xs

