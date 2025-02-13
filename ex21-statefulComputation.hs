newtype State s a =
    State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap :: (a -> b)
      -> State s a -> State s b
    fmap f (State g) = State $ \s ->
        let (x, s') = g s
        in (f x, s')

instance Applicative (State s) where
    pure :: a -> State s a
    pure x = State $ \s -> (x, s)

    (<*>) :: State s (a -> b) -> State s a
      -> State s b
    State h <*> State g = State $ \s ->
        let (f, s')   = h s
            (x', s'') = g s'
        in (f x', s'')

instance Monad (State s) where
    (>>=) :: State s a -> (a -> State s b)
      -> State s b
    State h >>= f = State $ \s ->
        let (x, s') = h s
            State g = f x
        in g s'

type Queue a = ([a], [a], Int)

size :: State (Queue a) Int
size = State $ \(f, b, n) -> (n, (f, b, n))

isEmpty :: State (Queue a) Bool
isEmpty = State $ \(f, b, n) -> (n == 0, (f, b, n))

enqueue :: a -> State (Queue a) ()
enqueue x = State $ \(f, b, n) -> ((), (f, x:b, n+1)) 

dequeue :: State (Queue a) a
dequeue = State $ \(f, b, n) -> 
  case f of
    (x:xs) -> (x, (xs, b, n-1))
    [] -> let revB = reverse b in case revB of
      (x:xs) -> (x, (xs, [], n-1))
      [] -> error "empty queue"

mkQueue :: [a] -> State (Queue a) ()
mkQueue xs = State $ \_ -> ((), (xs, [], length xs))

empty :: State (Queue a) ()
empty = State $ \_ -> ((), ([], [], 0))

joinState :: State s (State s a) -> State s a
joinState (State a) = State $ \s ->
  let (State b, s') = a s
  in b s'

-- prove that state monad satisfies functor laws
-- 1) fmap id = id
--    fmap id (State g) from definition of fmap will get
--    State $ \s -> let (x, s') = g s in (id x, s')
--    so, fmap id (State g) = \s -> (x, s')
--    that is State g
--    and id (State g) = State g
--  so, fmap id = id

-- 2) fmap (f . g) = fmap f . fmap g

--    start from fmap f . fmap g (State h), we will get 
--    fmap f (fmap g (State h)), from definition of fmap
--    fmap f (State $ \s -> let (x, s') = h s in (g x, s')), from definition of fmap
--      we will get State (\s -> (g x, s')) from apply fmap g (State h) 
--    and apply fmap f to State (\s -> (g x, s')) we will get
--      State $ \s -> let (y, s'') = (\s -> (g x, s')) s in (f y, s''), we know that t = g x and s'' is s'
--    so, we will get 
--      State $ \s -> (f (g x), s')

--    next, fmap (f . g) (State h), from definition of fmap
--    State $ \s -> let (x, s') = h s in ((f . g) x, s'), apply (f.g) to x we will get
--    State $ \s -> let (x, s') = h s in (f (g x), s'), so we will get
--    State $ \s -> (f (g x), s')
--    that means, fmap (f.g) = fmap f . fmap g

