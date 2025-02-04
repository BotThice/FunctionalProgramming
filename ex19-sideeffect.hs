-- no limit round to guess
-- impure parts
getP1Number :: IO Integer
getP1Number = do 
    putStrLn("Enter the number for P2 to guess.")
    number <- getLine
    return (read number)

getP2Guess :: IO Integer
getP2Guess = do
    guess <- getLine
    return (read guess)

p2Guess :: Integer -> IO ()
p2Guess p1Num = do
    guess <- getP2Guess

    if isCorrect p1Num guess then
        putStrLn("Correct!")
    else do
        putStrLn("Incorrect. Try again.")
        p2Guess p1Num

-- pure parts
isCorrect :: Integer -> Integer -> Bool
isCorrect p1Num guessNum = p1Num == guessNum

main1 :: IO ()
main1 = do
    p1Num <- getP1Number
    putStrLn("P2, guess the number.")
    p2Guess p1Num


-- with limited rounds
-- impure parts
getP1Round :: IO Integer
getP1Round = do
    putStrLn("Enter the number of rounds for P2 to guess.")
    rounds <- getLine
    return (read rounds)

p2GuessRound :: Integer -> Integer -> IO ()
p2GuessRound p1Num rounds = do
    if isOutofRounds rounds then
        putStrLn("P2 has run out of rounds.")
    else do
        guess <- getP2Guess

        if isCorrect p1Num guess then
            putStrLn("Correct!")
        else do
            putStrLn("Incorrect. Try again.")
            p2GuessRound p1Num (decreaseBy1 rounds)

-- pure parts
isOutofRounds :: Integer -> Bool
isOutofRounds remain = remain == 0

decreaseBy1 :: Integer -> Integer
decreaseBy1 x = x - 1

main2 :: IO ()
main2 = do
    p1Num <- getP1Number
    rounds <- getP1Round
    putStrLn("P2, guess the number.")
    p2GuessRound p1Num rounds
    
-- this version can reuse some part of previous version.
-- Include all pure parts of previos version. (isCorrect)
-- Moreover, we can reuse getP1Number, getP2Guess.