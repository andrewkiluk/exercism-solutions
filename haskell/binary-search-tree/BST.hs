module BST (bstLeft, 
           bstRight, 
           bstValue, 
           empty, 
           singleton, 
           insert, 
           fromList, 
           toList) where

data Tree a = Empty | Node {value :: a, leftChild :: Tree a, rightChild :: Tree a} deriving (Show,Eq)

empty :: Tree a
empty = Empty

singleton :: a -> Tree a
singleton x = Node x Empty Empty

fromList :: (Ord a) => [a] -> Tree a
fromList = foldl (flip insert) Empty

toList :: Tree a -> [a]
toList Empty = []
toList (Node v l r) = toList l ++ [v] ++ toList r

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node v l r) | x <= v    = Node v (insert x l) r
                      | otherwise = Node v l (insert x r) 

-- Accessors

bstValue :: Tree a -> Maybe a
bstValue Empty         = Nothing
bstValue Node{value=v} = Just v

bstLeft :: Tree a -> Maybe (Tree a)
bstLeft Empty             = Nothing
bstLeft Node{leftChild=l} = Just l

bstRight :: Tree a -> Maybe (Tree a)
bstRight Empty              = Nothing
bstRight Node{rightChild=r} = Just r

