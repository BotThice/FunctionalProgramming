type CourseID = Int
type StudentID = Int
type Capacity = Int

data CourseInfo' = Course'
    { cid::CourseID
    , cap::Capacity
    , roster::[StudentID] } deriving (Show)

-- enroll student in the course if
-- possible and return updated course
-- otherwise, Nothing
enroll'' :: CourseInfo' -> StudentID ->
        Either String CourseInfo'
enroll'' c sid
  | sid `elem` rs =
      Left "student already registered"
  | length rs >= seats =
      Left "course full"
  | otherwise = Right $
      Course' (cid c) seats (sid:rs)
  where seats = cap c
        rs = roster c
-- register student in a given course and
-- return updated registration information
-- if possible
register'' :: [CourseInfo']
    -> CourseID -> StudentID
    -> Either String [CourseInfo']
register'' [] _ _ = Left "no such course"
register'' (c : cs) cid' sid
  | cid c == cid' =
      case enroll'' c sid of
        Left msg -> Left msg
        Right c' -> Right (c' : cs)
  | otherwise = register'' cs cid' sid

allCourse :: [CourseInfo']
allCourse = [ (Course' 261409 20 [650610762]), (Course' 261408 21 []), (Course' 261407 19 [650610762]) ]
-- bug case : updatedCourse = register'' allcourse 261407 650610666
-- if you show the value in updatedCourse you will get [Course' {cid = 261407, cap = 19, roster = [650610666, 650610762]}]
-- all the previous course in list before 261407 are lost.

-- i have no idea it is a bug or wrong use
-- Right allCourse = register'' allcourse 261407 650610666
-- it will be infinity loop i think

-- fixed bug version

register''' :: [CourseInfo']
    -> CourseID -> StudentID
    -> Either String [CourseInfo']
register''' [] _ _ = Left "no such course"
register''' cs cid' sid = register_aux cs cid' sid []
    where 
        register_aux (c:cs) cid' sid prev
            | cid c == cid' =
                case enroll'' c sid of
                    Left msg -> Left msg
                    Right c' -> Right (prev ++ (c' : cs)) 
            | otherwise = register_aux cs cid' sid (c:prev)


maybeAp :: Maybe (a -> b) -> Maybe a -> Maybe b
maybeAp Nothing _ = Nothing
maybeAp _ Nothing = Nothing
maybeAp (Just f) (Just a) = Just (f a)

testMaybeAp1 = maybeAp Nothing (Just 3)
testMaybeAp2 = maybeAp (Just (*2)) Nothing
testMaybeAp3 = maybeAp (Just (*2)) (Just 3)
testMaybeAp4 = maybeAp (maybeAp (Just (-)) (Just 25)) (Just 47)

initMaybe :: a -> Maybe a
initMaybe a = Just a

listAp :: [a -> b] -> [a] -> [b]
listAp fs as = [f x | f <- fs, x <- as]

testListAp1 = listAp [] [1,2]         
testListAp2 = listAp [(*2),(+5)] []   
testListAp3 = listAp [(*2),(+5)] [1]  
testListAp4 = listAp [(*2)] [1,2]     
testListAp5 = listAp [(*2),(+5)] [1,2]

initList :: a -> [a]
initList a = [a]

-- explain what fmap (*3) (+100) do
-- first type of fmap (*3) (+100) is Num b => b -> b
-- that's mean it want 1 type b Argument and return b
-- fmap will apply (*3) with (+100) which is partial applied
-- so, if you apply 1 more Num argument named x
-- it will apply x with (+100) first 
-- then the result of (+100) x will be applied with (*3)
-- you will get ((x + 100) * 3)
-- example : fmap (*3) (+100) 6
-- you will get (6 + 100) * 3 = 318
