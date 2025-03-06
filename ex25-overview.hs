pairBimap :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
pairBimap f g (x, y) = (f x, g y)

-- prove that these bifunctor laws hold:

-- pairBimap id id = id

-- pairBimap id id (a, b)
-- from definition of pairBimap we'll get
-- pairBimap id id (a, b) = (id a, id b)
-- we know that id a = a, id b = b
-- so, (id a, id b) = (a, b) = id (a, b)
-- so that, pairBimap id id = id holds.

-- pairBimap (f . g) (h . i) = pairBimap f h . pairBimap g i

-- pairBimap (f . g) (h . i) (x, y)
-- from definition
-- pairBimap (f . g) (h . i) (x, y) = ((f . g) x, (h . i), y)
-- ((f . g) x, (h . i), y) = (f(g(x)), h(i(y)))
-- from definition (f x, g y) = pairBimap f g (x, y)
-- (f( g(x) ), h( i(y) )) = pairBimap f h (g x, i y)
-- from definition (f x, g y) = pairBimap f g (x, y)
-- (g x, i y) = pairBimap g i (x, y)
-- therefore, pairBimap f h (pairBimap g i (x, y))
-- we can rewrite to pairBimap f h . pairBimap g i (x, y)
-- thus, pairBimap (f . g) (h . i) = pairBimap f h . pairBimap g i holds.

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]
-- true or false, and why: for any f :: a -> b,
-- fmap f . maybeToList = maybeToList . fmap f

-- case 1: Nothing
--  fmap f . maybeToList Nothing
--  from definition maybeToList Nothing = []
--  so, we get fmap f []
--  from definition of fmap list
--  fmap f [] = []

--  maybeToList . fmap f Nothing
--  from definition of famp Maybe 
--  maybeToList . fmap f Nothing = maybeToList Nothing
--  from definition of maybeToList
--  maybeToList Nothing = []
    -- hence, fmap f . maybeToList Nothing = [] = maybeToList . fmap f Nothing
    -- case 1 holds.

-- case 2: Just x
-- f :: a -> b, assume x has type a, y has type b
--  fmap f . maybeToList (Just x)
--  from definition maybeToList 
--  fmap f . maybeToList (Just x) = fmap f [x]
--  from definition of fmap list 
--  fmap f [x] = [y], because of [x] has only one elem
--  from definition of maybeToLit
--  since, [y] has only one elem 
--  [y] = maybeToList (Just y) = maybeToList . Just y

--  maybeToList . fmap f (Just x)
--  from definition of fmap Maybe
--  maybeToList . fmap f (Just x) = maybeToList . Just (f x)
--  maybeToList . Just y
    -- To summarize, fmap f . maybeToList (Just x) = maybeToList . fmap f (Just x)
    -- case 2 holds
-- since, both cases hold.
-- fmap f . maybeToList = maybeToList . fmap f is True for any f :: a -> b


listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x

-- true or false, and why: for any f :: a -> b,
-- fmap f . listToMaybe = listToMaybe . fmap f

-- case 1: []
-- fmap f . listToMaybe []
-- from definition of listToMaybe
-- fmap f . listToMaybe [] = fmap f Nothing
-- from definition of fmap of Maybe
-- fmap f Nothing = Nothing
-- from definition of listToMaybe
-- Nothing = listToMaybe []
-- from definition of fmap list we know that fmap _ [] = []
-- so, listToMaybe [] = listToMaybe . fmap f []
-- case 1 holds.

-- case 2: (x:xs)
-- assume (x:xs) has type [a]
-- fmap f . listToMaybe (x:xs)
-- from definition of listToMaybe
-- fmap f . listToMaybe (x:xs) = fmap f (Just x)
-- from definition of famp Maybe
-- fmap f (Just x) = Just (f x)

-- listToMaybe . fmap f (x:xs)
-- from definition of fmap list 
-- listToMaybe . fmap f (x:xs) = listToMaybe (f x : fmap f xs)
-- from definiion of listToMaybe
-- listToMaybe (f x : fmap f xs) = Just (f x)
-- so, fmap f . listToMaybe (x:xs) = listToMaybe . fmap f (x:xs)
-- case 2 holds.
-- since, both cases hold.
-- fmap f . listToMaybe = listToMaybe . fmap f is true for any f :: a -> b
