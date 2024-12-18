-- list.hs

len [] = 0
len (x:xs) = 1 + len xs

join [] [] = []
join [] (x:xs) = x:xs 
join (x:xs) (y) = x: join xs y

join1 [] (y) = y
join1 (x:xs) (y) = x : join xs y