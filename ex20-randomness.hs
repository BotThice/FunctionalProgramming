import System.Random
import Text.Read


readTarget = readNumber "Target number"

readNumber msg = do
    putStr $ msg ++ ": "
    line <- getLine
    case readEither line :: Either String Int of
        Left e -> do
            putStrLn e
            readNumber msg
        Right n -> return n

randomNumber = do
    g <- newStdGen
    return (fst (uniformR (0, 1000)  g))

verdict target guess = do
    case compare guess target of
        EQ -> Right "You win!"
        LT -> Left "Too low"
        GT -> Left "Too high"

validGuess guess rightNum (low, high) = 
    if guess > low && guess <= rightNum
    then Right (guess, high)
    else if guess < high && guess > rightNum
    then Right (low, guess)
    else Left "Invalid guess"

runGame2 num count cont validRange = do
    guess <- readNumber "Guess"
    let newRange = validGuess guess num validRange
    case newRange of
        Right (low, high) -> do
            let v = verdict num guess
            case v of
                Right m -> do
                    putStrLn m
                Left m -> do
                    putStrLn m
                    if cont count
                    then runGame2 num (count+1) cont (low, high)
                    else putStrLn "Game over"
        Left m -> do
            putStrLn m
            runGame2 num count cont validRange

v3 :: IO ()
v3 = do
    num <- randomNumber
    lim <- readNumber "Guess limit"
    runGame2 num 1 (<lim) (minBound, maxBound)

nRandomRs :: (RandomGen g, UniformRange a, Integral n) => (a, a) -> n -> g -> ([a], g)
nRandomRs range n g = nRandAux range n g []
    where
        nRandAux :: (RandomGen g, UniformRange a, Integral n) => (a, a) -> n -> g -> [a] -> ([a], g)
        nRandAux _ 0 g acc = (acc, g)
        nRandAux range n g acc = let (x, g') = uniformR range g in nRandAux range (n-1) g' (x:acc)

main :: IO ()
main = do
    let range = (1, 100)
    let count = 10
    g <- newStdGen
    let (randomNumbers, _) = nRandomRs range count g :: ([Int], StdGen)
    print randomNumbers