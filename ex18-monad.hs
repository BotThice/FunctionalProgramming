maybeJoin :: Maybe (Maybe a) -> Maybe a
maybeJoin Nothing = Nothing
maybeJoin (Just x) = x

listJoin :: [[a]] -> [a]
listJoin ls = concat ls

eitherJoin :: Either r (Either r a) -> Either r a
eitherJoin (Left x) = Left x
eitherJoin (Right x) = x

arrowJoin :: (r -> r -> a) -> r -> a
arrowJoin f x = f x x

pairJoin :: (Semigroup r) => (r, (r, a)) -> (r, a)
pairJoin (r1, (r2, x)) = (r1 <> r2, x)


-- prove that the three monad laws hold for the Either monad
-- hint: prove by cases (Left vs Right)
-- instance Monad (Either e) where
--     Right m >>= k = k m
--     Left e  >>= _ = Left e

--  Law 1 (Left identity) return a >>= k = k a
--      return a >>= k ,(return a = Right a)
--      Right a >>= k
--      (Right a) >>= k = k a

--  Law 2 (Right identity) m >>= return = m
--      case 1:
--              (Left e) >>= return , from definition Left e >>= _ = Left e
--              so, (Left e) >>= return = Left e 
--      case 2:
--              (Right m) >>= return , from definition Right m >>= k = k m
--              so, (Right m) >>= return = return m = Right m
-- both case m >>= return = m Law 2 holds

--  Law 3 (Associativity) m >>= (\x -> k x >>= h) = (m >>= k) >>= h
--      case 1:
--              (Left e) >>= (\x -> k x >>= h), from Left e >>= _ = Left e so,
--              (Left e) >>= (\x -> k x >>= h) = Left e

--              ((Left e) >>= k) >>= h, from Left e >>= _ = Left e so,
--              ((Left e) >>= k) >>= h 
--              = Left e >>= h 
--              = Left e
-- case Left e holds

--      case 2:
--              (Right m) >>= (\x -> k x >>= h), from Right m >>= k = k m so, 
--              (\x -> k x >>= h) m 
--              = k m >>= h

--              ((Right m) >>= k) >>= h, from Right m >>= k = k m so,
--              = k m >>= h
-- case Right m holds
-- because both cases hold, Law 3 holds



-- prove that the three monad laws hold for the list monad
-- instance Monad []  where
-- 	xs >>= f = [y | x <- xs, y <- f x]

--  Law 1 (Left identity) return a >>= k = k a
--      return a >>= k, from return a = [a]
--      [a] >>= k, from definition [y | x <- xs, y <- f x]
--      [y | x <- [a], y <- f x], since x <- [a] is a, so
--      [y | a <- [a], y <- f a] 
--      [y | y <- f a] = f a
-- So, Law 1 holds

--  Law 2 (Right identity) m >>= return = m
--      xs >>= return, from definition xs >>= f = [y | x <- xs, y <- f x]
--      [y | x <- xs, y <- return x]
--      [y | x <- xs, y <- [x]] = xs
-- So, Law 2 holds

--  Law 3 (Associativity) m >>= (\x -> k x >>= h) = (m >>= k) >>= h
--      xs >>= (\x -> k x >>= h), from definition xs >>= f = [y | x <- xs, y <- f x]
--      [y | x <- xs, y <- (\x -> k x >>= h) x]
--      [y | x <- xs, y <- k x >>= h]
--      [z | y <- [y | x <- xs, y <- k x], z <- h y]

--      (xs >>= k) >>= h, from definition xs >>= f = [y | x <- xs, y <- f x]
--      ([y | x <- xs, y <- k x]) >>= h
--      [z | y <- [y | x <- xs, y <- k x], z <- h y]
-- So, Law 3 holds

-- prove that the three monad laws hold for the arrow monad
-- hint: unlock each side of the equality with a value of type r, and check that both sides are indeed equal
-- instance Monad ((->) r) where
-- 	f >>= k = \ r -> k (f r) r

--  Law 1 (Left identity) return a >>= k = k a
--      return a >>= k, from return a = \r -> a 
--      (\r -> a) >> = k, from f >>= k = \r -> k (f r) r
--      \r -> k ((\r -> a) r) r, because (\r -> a) r = a
--      \r -> k a r, eta reduction r
--      k a
-- So, Law 1 holds

--  Law 2 (Right identity) m >>= return = m
--      assume f :: r -> a
--      f >>= return, from f >>= k = \r -> k (f r) r  
--      \r -> return (f r) r, since (f r) = a
--      \r -> return a r, eta reduction r
--      return a, since return a = \r -> a
--      \r -> a, 
--      from f :: r -> a we can rewrite f in f = \r -> a
--      so, \r -> a = f
-- Law 2 holds

-- Law 3 (Associativity) m >>= (\x -> k x >>= h) = (m >>= k) >>= h
--      assume m :: r -> a, k :: a -> (r -> b), h :: b -> (r -> c)
--      m >>= (\x -> k x >>= h), from f >>= k = \r -> k (f r) r
--      \r -> (\x -> k x >>= h) (m r) r, from m :: r -> a so, m r = a
--      \r -> (\x -> k x >>= h) a r, eta reduction r
--      (\x -> k x >>= h) a, apply a
--      k a >>= h    

--      (m >>= k) >>= h, from f >>= k = \r -> k (f r) r 
--      (\r -> k (m r) r) >>= h, from m r = a 
--      \r -> k a r >> h, eta reduction r
--      k a >>=h 
-- So, Law 3 holds