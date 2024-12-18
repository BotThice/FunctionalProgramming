-- join :: [a] -> [a] -> [a]
-- join [] [] = []
-- join [] x = x
-- join (x:xs) y = x: join xs y

join :: ([a], [a]) -> [a]
join ([], y) = y
join (x:xs, y) = x : join (xs, y)

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = join (rev xs, [x])

-- how long does join take
    -- join takes O(n) times on list size n
-- how long does reverse take
    -- reverse takes O(n^2) om lise size n
-- are you satisfied with the running time?
    -- I think its okay to do reverse list with O(n^2)
