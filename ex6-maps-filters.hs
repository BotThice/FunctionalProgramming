{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}

take_While :: (a -> Bool) -> [a] -> [a]
take_While predicate (a:as)
    | predicate a = a: take_While predicate as
    | otherwise = []

lessThan3Len :: Foldable t => t a -> Bool
lessThan3Len l = ((<3).length) l

filterConcat :: ([a] -> Bool) -> [[a]] -> [a]
filterConcat predicate (a:as) = concatList (filter_aux (a:as))
    where 
        filter_aux [] = []
        filter_aux (a:as)
            | predicate a = a: filter_aux as
            | otherwise = filter_aux as 
        concatList (a:as) = a ++ concatList as
        concatList [] = []


-- avoid recursion version
filterConcat' :: ([a] -> Bool) -> [[a]] -> [a]
filterConcat' predicate (a:as) = (concat.filter predicate) (a:as)