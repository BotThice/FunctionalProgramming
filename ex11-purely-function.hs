data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show)
data NAryTree a = NEmpty | NNode a [NAryTree a] deriving (Show)
  

mapBTree :: (a -> b) -> Tree a -> Tree b
mapBTree _ Empty = Empty
mapBTree op (Node l root r) = Node (mapBTree op l) (op root) (mapBTree op r)


height :: Tree a -> Integer
height Empty = 0
height (Node l root r) = max (height l) (height r) + 1

-- foldTree0 (\root l r -> root ++ l ++ r) "" tree4
foldTree0  :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree0 _ acc Empty = acc
foldTree0 op acc (Node l root r) = op root (foldTree0 op acc l) (foldTree0 op acc r)

-- foldTree1 (++) "" tree4
foldTree1 :: (a -> b -> b) -> b -> Tree a -> b
foldTree1 func acc Empty = acc
foldTree1 func acc (Node l root r) = func root (foldTree1 func (foldTree1 func acc r) l)

foldTree2 :: (b -> a -> b) -> b -> Tree a -> b
foldTree2 func acc Empty = acc
foldTree2 func acc (Node l root r) = func (foldTree2 func (foldTree2 func acc l) r) root

preOrderNAry :: NAryTree a -> [a]
preOrderNAry NEmpty = []
preOrderNAry (NNode root []) = [root]
preOrderNAry (NNode root children) = root : concatMap preOrderNAry children

postOrderNAry :: NAryTree a -> [a]
postOrderNAry NEmpty = []
postOrderNAry (NNode root []) = [root]
postOrderNAry (NNode root children) = concatMap postOrderNAry children ++ [root]

isBST :: Ord a => Tree a -> Bool
isBST Empty = True
isBST (Node Empty root Empty) = True
isBST (Node Empty root r)
    | findMin r < root = False
    | otherwise = isBST r
isBST (Node l root Empty)
    | findMax l > root = False
    | otherwise = isBST l
isBST (Node l root r)
    | findMax l > root = False
    | findMin r < root = False
    | otherwise = isBST l && isBST r
    
findMin :: Ord a => Tree a -> a
findMin (Node Empty root Empty) = root
findMin (Node l root Empty) = min root (findMin l)
findMin (Node Empty root r) = min root (findMin r)
findMin (Node l root r) = min root (min (findMin l) (findMin r))

findMax :: Ord a => Tree a -> a
findMax (Node Empty root Empty) = root
findMax (Node l root Empty) = max root (findMax l)
findMax (Node Empty root r) = max root (findMax r)
findMax (Node l root r) = max root (max (findMax l) (findMax r))

tree1 = Node (Node (Node Empty 1 Empty) 3 (Node (Node Empty 4 Empty) 6 (Node Empty 7 Empty))) 8 (Node Empty 10 (Node (Node Empty 13 Empty) 14 Empty))
tree2 = Node (Node (Node Empty 2 Empty) 3 (Node (Node Empty 4 Empty) 6 (Node Empty 7 Empty))) 8 (Node Empty 10 (Node (Node Empty 1 Empty) 14 Empty))
tree3 = Node (Node (Node Empty 20 Empty) 3 (Node (Node Empty 4 Empty) 6 (Node Empty 7 Empty))) 8 (Node Empty 10 (Node (Node Empty 1 Empty) 14 Empty))
tree4 = Node (Node Empty "e" Empty) "h" (Node Empty "l" Empty)