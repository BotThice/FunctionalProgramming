-- contain1 = \x l -> any (x<) l
contain1' :: (Foldable t, Ord a) => a -> t a -> Bool
contain1' =  any.(<)

-- contain2 = \l x -> any (x<) l
contain2' :: (Foldable t, Ord a) => t a -> a -> Bool
contain2' = (.(<)) . (flip any)


lenComp :: Num a => [b] -> a
lenComp a = sum [1 | x <- a] 


x = [2,3,5]
y = [1,2,4]
sumIsEvenTuple :: (Integral b) => [b] -> [b] -> [(b, b)]
sumIsEvenTuple (x:xs) (y) = filter sumEven (allPairGen (x:xs) y)
    where   
        allPairGen (x:xs) (y) = map (x,) y ++ allPairGen xs y 
        allPairGen [] _ = []
        sumEven (a, b) = even (a + b)


-- note :
-- contain2' l = (.(<)) (flip any l)
-- contain2' l = ((.(<)).(flip any)) l
