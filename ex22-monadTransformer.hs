import Data.Char

class MonadTrans t where
  lift :: Monad m => m a -> t m a

-- implement a monad transformer for Identity monad:
-- IdentityT m a, wrapping m a
newtype IdentityT m a = IdentityT { runIdentityT :: m a }

instance MonadTrans IdentityT where
  lift x = IdentityT x

instance Functor m => Functor (IdentityT m) where
    fmap f (IdentityT x) = lift (fmap f x)

instance Applicative m => Applicative (IdentityT m) where
    pure x = lift (pure x)

    (IdentityT mf) <*> (IdentityT mx) = lift (mf <*> mx)

instance Monad m => Monad (IdentityT m) where
    (IdentityT mx) >>= f = lift $ do
        x <- mx
        runIdentityT (f x)

-- implement a monad transformer for Either monad:
-- EitherT a m b, wrapping m (Either a b)
newtype EitherT a m b = EitherT { runEitherT :: m (Either a b) }

instance MonadTrans (EitherT m) where
    lift ma = EitherT $ do
        a <- ma
        return (Right a) 

instance Functor m => Functor (EitherT a m) where
    fmap f (EitherT x) = EitherT (fmap (fmap f) x)

instance Applicative m => Applicative (EitherT a m) where
    pure x = EitherT (pure (Right x)) 

    (EitherT mf) <*> (EitherT mx) = EitherT $ do
        fmap apply mf <*> mx 
        where 
            apply (Left e) _ = Left e
            apply _ (Left e) = Left e
            apply (Right f) (Right x) = (Right (f x))

instance Monad m => Monad (EitherT a m) where
    (EitherT mx) >>= f = EitherT $ do
        x <- mx
        case x of 
            Left a -> return (Left a)
            Right b -> runEitherT (f b)

-- implement a monad transformer for the arrow monad:
-- ContT r m a, wrapping m (r -> a)
newtype ContT r m a = ContT { runContT :: m (r -> a) }

instance MonadTrans (ContT r) where
    lift m = ContT $ do
        a <- m
        return (\_ -> a)

instance Functor m => Functor (ContT r m) where
    fmap f (ContT x) = ContT (fmap (fmap f) x)

instance Applicative m => Applicative (ContT r m) where
    pure x = ContT (pure (\_ -> x))

    (ContT mf) <*> (ContT mx) = ContT $ 
        fmap apply mf <*> mx
        where
            apply f x = \r -> f r (x r)

instance Monad m => Monad (ContT r m) where
    (ContT mx) >>= f = ContT $ do
        rx <- mx
        runContT (\r -> (f (rx r))) 

-- modify the signup page example to use EitherT transformer
readEmail :: IO (Either String String)
readEmail = do
  putStrLn "Please enter your email!"
  str <- getLine
  if '@' `elem` str && '.' `elem` str
    then return $ Right str
    else return $ Left "Incorrect email format"

readPassword :: IO (Either String String)
readPassword = do
  putStrLn "Please enter your Password!"
  str <- getLine
  if length str < 8
    then return $ Left "password must have 8 or more character"
  else if null (filter isUpper str) || null (filter isLower str)
    then return $ Left "password must contain capital and small letter"
  else return $ Right str

readEmail' :: EitherT String IO String
readEmail' = EitherT readEmail

readPassword' :: EitherT String IO String
readPassword' = EitherT readPassword

signup' :: EitherT String IO (String, String)
signup' = do
  email <- readEmail'
  password <- readPassword'
  password2 <- readPassword'
  if password == password2
  then EitherT . return $ Right (email, password)
  else EitherT . return $ Left "password does not match"

main :: IO ()
main = do
  signupRes <- runEitherT signup'
  case signupRes of
    Left e -> putStrLn e
    Right _ -> putStrLn "Signup success"