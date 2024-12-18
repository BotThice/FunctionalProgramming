zipper :: [a] -> [b] -> [(a, b)]
zipper a b = zipper_aux a b []
    where
        zipper_aux [] _ res = res
        zipper_aux _ [] res = res
        zipper_aux (a:as) (b:bs) res = zipper_aux as bs (res ++ [(a, b)])

-- List mapping
listMap func [] = []
listMap func (a:as) = func a : listMap func as
-- type of listMap is listMap (a -> b) -> [a] -> [b]
-- tail listMap
tailListMap func a = tailListMap_aux func a []
    where
        tailListMap_aux func [] res = res 
        tailListMap_aux func (a:as) res = tailListMap_aux func as (res ++ [func a])


-- 3 more test cases
    -- listMap ("Hello World! " ++) ["FP", "See U", ""]
    -- listMap (/0) [1, 2, 3, 4]
    -- facs = 1 : zipWith (*) [1..] facs
    -- listMap (\x -> take x facs) [1,2,3,4]

-- challenge
fibo = 0 : 1 : zipWith (+) fibo (tail fibo)
