-- reverse left fold version
reverse_l :: Foldable t => t a -> [a]
-- reverse_l list = foldl (flip (:)) [] list 
reverse_l = foldl (flip (:)) []

-- reverse right fold version
reverse_r :: [a] -> [a]
-- reverse_r list = foldr (\l acc -> acc ++ [l]) [] list
reverse_r  = foldr (\l acc -> acc ++ [l]) []
-- which version is more efficient
-- reverse_l is more efficient because of this version has tail recursion concept avoid long chain of stack frames.
-- reverse_l has O(n) but reverse_r has O(n^2)

-- map using fold
map' :: (a -> b) -> [a] -> [b]
-- map' partialFunc list = foldr (\l acc -> partialFunc l : acc) [] list
map' partialFunc = foldr (\l acc -> partialFunc l : acc) []

-- filter using fold
filter' :: (a -> Bool) -> [a] -> [a]
-- filter' pred list = foldr (\l acc ->  if pred l then l:acc else acc) [] list
filter' pred = foldr (\l acc ->  if pred l then l:acc else acc) []