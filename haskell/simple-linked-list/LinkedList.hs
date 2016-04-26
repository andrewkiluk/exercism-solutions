module LinkedList where

data List a = Empty | Node { datum :: a, next :: List a }

nil = Empty :: List a

new :: a -> List a -> List a
new x xs = Node x xs

isNil :: List a -> Bool
isNil Empty = True
isNil _     = False

fromList :: [a] -> List a
fromList []     = Empty
fromList (x:xs) = Node x (fromList xs)

toList :: List a -> [a]
toList Empty       = []
toList (Node x xs) = x:toList xs

reverseLinkedList :: List a -> List a
reverseLinkedList Empty  = Empty
reverseLinkedList (Node x xs) = reverseLinkedList xs LinkedList.++ Node x Empty

(++) :: List a -> List a -> List a
(++) Empty ys = ys
(++) (Node x xs) ys = Node x (xs LinkedList.++ ys)
