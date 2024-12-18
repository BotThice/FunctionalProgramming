zipper:: ([a], [b]) -> [(a, b)]
zipper ([], _) = []
zipper (_, []) = []
zipper (a:as, b:bs) = (a, b): zipper(as, bs)

zipper' :: [a] -> [b] -> [(a, b)]
zipper' [] _ = []
zipper' _ [] = []
zipper' (a:as) (b:bs) = (a, b): zipper(as, bs)

-- type of zipper' is zipper' :: [a] -> [b] -> [(a, b)]
-- type of zipper' [] is zipper' [] :: [b] -> [(a,b)]
-- zipper' [] will receive [b] then return []
-- another way to describe is zipper' [] will discard [b]
