
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition predicate [] = ([], [])
partition predicate (x:xs)
  | predicate x       = (x:l, r)
  | otherwise = (l, x:r)
  where (l, r) = partition predicate xs

-- type of partition is 
        -- partition :: (a -> Bool) -> [a] -> ([a], [a])
-- this partition will split given list into two list by using predicate.
    -- the result list will be pair of list one is list of x that apply with predicate and result is True another is False

filter' :: (a -> Bool) -> [a] -> [a]
filter' predicate list = fst (partition predicate list)

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort (lessThan) ++ [x] ++ quickSort (moreThan)
    where (lessThan, moreThan) = partition (<x) xs

-- Ordering
-- Ordering use to represent the result of comparing
-- has 3 constructors (LT, EQ, GT)
-- I think Ordering has 3 way to matching

gpa :: Fractional a => [String] -> a
gpa (gradeList) = gpaCalculation (weightCalculation (convertNum noWList) 1) (len noWList)
    where
        noWList = filter (/= "W") gradeList
        convertNum [] = []
        convertNum (l:ls)
            | l == "A" = 4.0 : convertNum ls
            | l == "B+" = 3.5 : convertNum ls
            | l == "B" = 3.0 : convertNum ls
            | l == "C+" = 2.5 : convertNum ls
            | l == "C" = 2.0 : convertNum ls
            | l == "D+" = 1.5 : convertNum ls
            | l == "D" = 1.0 : convertNum ls
            | l == "F" = 0.0 : convertNum ls
        weightCalculation [] _ = 0
        weightCalculation (grade:remain) (credit) = grade*credit + weightCalculation remain credit
        gpaCalculation (allGrade) (totalcredit) = allGrade/totalcredit
        len [] = 0
        len (_:xs) = 1 + len xs
