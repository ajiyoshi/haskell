module TreeSet
( tree
, entry
, leftBranch
, rightBranch
, elementOfSet
, adjoinSet
, treeToList
, treeToList'
, listToTree
, unionSet
, intersectionSet
) where

import qualified Set as S

data Tree a = Leaf a | Branch a (Tree a) (Tree a) | Empty deriving (Show) 

tree :: Ord a => a -> Tree a -> Tree a -> Tree a
tree e Empty Empty = Leaf e
tree e lb rb = Branch e lb rb

entry :: Ord a => Tree a -> a
entry (Leaf e) = e
entry (Branch e _ _) = e

leftBranch :: Ord a => Tree a -> Tree a
leftBranch (Branch _ left _) = left
leftBranch (Leaf _) = Empty

rightBranch :: Ord a => Tree a -> Tree a
rightBranch (Branch _ _ right) = right
rightBranch (Leaf _) = Empty

elementOfSet :: Ord a => a -> Tree a -> Bool
elementOfSet _ Empty = False
elementOfSet x (Leaf a) = x == a
elementOfSet x (Branch e l r)
    | x == e = True
    | x <  e = elementOfSet x l
    | x >  e = elementOfSet x r

adjoinSet :: Ord a => a -> Tree a -> Tree a
adjoinSet x Empty = Leaf x
adjoinSet x l@(Leaf e) 
    | x == e = l
    | x <  e = Branch e (Leaf x) Empty
    | x >  e = Branch e Empty (Leaf x)
adjoinSet x branch@(Branch e l r)
    | x == e = branch
    | x <  e = Branch e (adjoinSet x l) r
    | x >  e = Branch e l (adjoinSet x r)

treeToList :: Ord a => Tree a -> [a]
treeToList Empty = []
treeToList (Leaf e) = [e]
treeToList (Branch e l r) = treeToList l ++ (e : treeToList r)

treeToList' :: Ord a => Tree a -> [a]
treeToList' t = itr t [] where
    itr Empty acc = acc
    itr (Leaf e) acc = e : acc
    itr (Branch e l r) acc = itr l (e: itr r acc)

listToTree :: Ord a => [a] -> Tree a
listToTree [] = Empty
listToTree es = fst $ partialTree es $ length es

partialTree :: Ord a => [a] -> Int -> (Tree a, [a])
partialTree es 0 = (Empty, es)
partialTree es n = (tree thisEntry leftTree rightTree, remaining) where
    leftSize = (n - 1) `div` 2
    (leftTree, thisEntry:nonLeft) = partialTree es leftSize
    rightSize = n - leftSize - 1
    (rightTree, remaining) = partialTree nonLeft rightSize

unionSet :: Ord a => Tree a -> Tree a -> Tree a
unionSet a b = listToTree $ S.unionSet as bs where
    as = treeToList' a
    bs = treeToList' b

intersectionSet :: Ord a => Tree a -> Tree a -> Tree a
intersectionSet a b = listToTree $ S.intersectionSet as bs where
    as = treeToList' a
    bs = treeToList' b
