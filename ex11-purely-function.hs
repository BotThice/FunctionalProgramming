data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show)
data NAryTree a = NEmpty | NNode a [NAryTree a] deriving (Show)
  

mapBTree :: (a -> b) -> Tree a -> Tree b
mapBTree _ Empty = Empty
mapBTree op (Node l root r) = Node (mapBTree op l) (op root) (mapBTree op r)


height :: Tree a -> Integer
height Empty = 0
height (Node l root r) = max (height l) (height r) + 1

foldTree  :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree _ acc Empty = acc
foldTree op acc (Node l root r) = op root (foldTree op acc l) (foldTree op acc r)




-- isBST :: Tree a -> Bool
-- isBST Empty = True
-- isBST (Node l root r) = mapBTree (<root) l && mapBTree (>root) r