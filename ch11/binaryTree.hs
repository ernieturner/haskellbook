module BinaryTree where

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)


insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
 | b == a = Node left a right
 | b < a = Node (insert' b left) a right
 | b > a = Node left a (insert' b right)


mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)


treeToListPreOrder :: BinaryTree a -> [a]
treeToListPreOrder Leaf = []
treeToListPreOrder (Node left a right) = [a] ++ (treeToListPreOrder left) ++ (treeToListPreOrder right)

treeToListInOrder :: BinaryTree a -> [a]
treeToListInOrder Leaf = []
treeToListInOrder (Node left a right) = (treeToListPreOrder left) ++ [a] ++ (treeToListPreOrder right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

listPreorder = [1, 3, 4]
listInOrder = [3, 1, 4]

treeToListPreorderOkay =
  if treeToListPreOrder testTree' == listPreorder
  then print "yup okay!"
  else error "test failed!"


treeToListInorderOkay =
  if treeToListInOrder testTree' == listInOrder
  then print "yup okay!"
  else error "test failed!"


foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node left a right) = f a $ foldTree f (foldTree f acc right) left

testFoldTree =
  if foldTree (+) 0 testTree' == 8
  then print "yup, okay!"
  else error "test failed!"