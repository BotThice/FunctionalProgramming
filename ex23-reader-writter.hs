import System.Random
import Text.Read

-- Writer part
newtype Writer w a = Writer {
  runWriter :: (a, w)
}

tell :: w -> Writer w ()
tell w = Writer ((), w)

instance Functor (Writer w) where
    fmap :: (a -> b) -> Writer w a -> Writer w b
    fmap f (Writer (a, w)) = Writer (f a, w)

instance Monoid w => Applicative (Writer w) where
    pure :: a -> Writer w a
    pure x = Writer (x, mempty)
    (<*>) :: Writer w (a -> b) -> Writer w a -> Writer w b
    (Writer (f, wf)) <*> (Writer (x, wx)) = Writer (f x, wf <> wx)

instance Monoid w => Monad (Writer w) where
    (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
    (Writer (a, wa)) >>= f = Writer (b, wa <> wb) 
        where Writer (b, wb) = f a

-- GuessList for logging guesses
newtype GuessList = GuessList { runGuessList :: [Int] }

instance Semigroup GuessList where
  (GuessList a) <> (GuessList b) = GuessList (a ++ b)

instance Monoid GuessList where
  mempty = GuessList []

instance Show GuessList where
  show (GuessList a) = show a

-- Guess game logics
readTarget :: IO Int
readTarget = readNumber "Target number"

readNumber :: String -> IO Int
readNumber msg = do
    putStr $ msg ++ ": "
    line <- getLine
    case readEither line :: Either String Int of
        Left e -> do
            putStrLn e
            readNumber msg
        Right n -> return n

randomNumber :: IO Int
randomNumber = do
    g <- newStdGen
    return (fst (uniformR (0, 1000)  g))

verdict :: Ord a => a -> a -> Either String String
verdict target guess = do
    case compare guess target of
        EQ -> Right "You win!"
        LT -> Left "Too low"
        GT -> Left "Too high"

validGuess :: Ord a => a -> a -> (a, a) -> Either String (a, a)
validGuess guess rightNum (low, high) = 
    if guess > low && guess <= rightNum
    then Right (guess, high)
    else if guess < high && guess > rightNum
    then Right (low, guess)
    else Left "Invalid guess"


runGame :: Int -> Int -> (Int -> Bool) -> (Int, Int) -> IO (Writer GuessList Bool)
runGame num count turn validRange = do
    guess <- readNumber "Guess"
    let newRange = validGuess guess num validRange

    case newRange of
        Right (low, high) -> do
            let v = verdict num guess
            case v of
                Right m -> do
                    putStrLn m
                    return (Writer (True, GuessList [guess])) -- Game won
                Left m -> do
                    putStrLn m
                    if turn count
                    then do
                        Writer (res, guesses) <- runGame num (count+1) turn (low, high)
                        return (Writer (res, GuessList [guess] <> guesses))
                    else return (Writer (False, GuessList [guess])) -- Game over
        Left m -> do
            putStrLn m
            runGame num count turn validRange

-- Main game function
v4 :: IO ()
v4 = do
    num <- randomNumber
    lim <- readNumber "Guess limit"
    Writer (gameResult, guesses) <- runGame num 1 (<lim) (minBound, maxBound)
    
    if gameResult
    then putStrLn "You win!"
    else putStrLn "You lose."

    putStrLn $ "Previous valid guesses: " ++ show guesses