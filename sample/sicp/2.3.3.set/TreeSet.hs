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

-- |
-- >>> tree 1 Empty Empty
-- Leaf 1
--
-- >>> tree 3 (Leaf 1) (Leaf 4)
-- Branch 3 (Leaf 1) (Leaf 4)
tree :: Ord a => a -> Tree a -> Tree a -> Tree a
tree e Empty Empty = Leaf e
tree e lb rb = Branch e lb rb

-- |
-- >>> entry $ Branch 3 (Leaf 1) (Leaf 4)
-- 3
--
-- >>> entry $ Leaf 2
-- 2
entry :: Ord a => Tree a -> a
entry (Leaf e) = e
entry (Branch e _ _) = e

-- |
-- >>> leftBranch (Leaf 2)
-- Empty
--
-- >>> leftBranch $ Branch 3 (Leaf 1) (Leaf 4)
-- Leaf 1
leftBranch :: Ord a => Tree a -> Tree a
leftBranch (Branch _ left _) = left
leftBranch (Leaf _) = Empty

-- |
-- >>> rightBranch (Leaf 2)
-- Empty
--
-- >>> rightBranch $ Branch 3 (Leaf 1) (Leaf 4)
-- Leaf 4
rightBranch :: Ord a => Tree a -> Tree a
rightBranch (Branch _ _ right) = right
rightBranch (Leaf _) = Empty

-- |
-- >>> let aTree = listToTree [1, 2, 3, 4, 5]
-- >>> elementOfSet 1 aTree
-- True
-- >>> elementOfSet 6 aTree
-- False
elementOfSet :: Ord a => a -> Tree a -> Bool
elementOfSet _ Empty = False
elementOfSet x (Leaf a) = x == a
elementOfSet x (Branch e l r)
    | x == e = True
    | x <  e = elementOfSet x l
    | x >  e = elementOfSet x r

-- |
-- >>> listToTree [1, 2, 3]
-- Branch 2 (Leaf 1) (Leaf 3)
--
-- >>> adjoinSet 1 . listToTree $ [1, 2, 3]
-- Branch 2 (Leaf 1) (Leaf 3)
--
-- >>> adjoinSet 4 . listToTree $ [1, 2, 3]
-- Branch 2 (Leaf 1) (Branch 3 Empty (Leaf 4))
--
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

-- |
-- >>> treeToList $ Branch 2 (Leaf 1) (Branch 3 Empty (Leaf 4))
-- [1,2,3,4]
--
-- >>> treeToList Empty
-- []
treeToList :: Ord a => Tree a -> [a]
treeToList Empty = []
treeToList (Leaf e) = [e]
treeToList (Branch e l r) = treeToList l ++ (e : treeToList r)

-- |
-- >>> treeToList' $ Branch 2 (Leaf 1) (Branch 3 Empty (Leaf 4))
-- [1,2,3,4]
treeToList' :: Ord a => Tree a -> [a]
treeToList' t = itr t [] where
    itr Empty acc = acc
    itr (Leaf e) acc = e : acc
    itr (Branch e l r) acc = itr l (e: itr r acc)

listToTree :: Ord a => [a] -> Tree a
listToTree [] = Empty
listToTree es = fst $ partialTree es $ length es

-- | リストの先頭n個から木を構築する
-- >>> partialTree [1, 2, 3] 1
-- (Leaf 1,[2,3])
--
-- >>> partialTree [1, 2, 3] 2
-- (Branch 1 Empty (Leaf 2),[3])
--
-- >>> partialTree [1, 2, 3] 3
-- (Branch 2 (Leaf 1) (Leaf 3),[])
partialTree :: Ord a => [a] -> Int -> (Tree a, [a])
partialTree es 0 = (Empty, es)
partialTree es n = (tree thisEntry leftTree rightTree, remaining) where
    leftSize = (n - 1) `div` 2
    (leftTree, thisEntry:nonLeft) = partialTree es leftSize
    rightSize = n - leftSize - 1
    (rightTree, remaining) = partialTree nonLeft rightSize

-- |
-- >>> unionSet (Branch 2 (Leaf 1) Empty) (Branch 3 (Leaf 2) (Leaf 4))
-- Branch 2 (Leaf 1) (Branch 3 Empty (Leaf 4))
unionSet :: Ord a => Tree a -> Tree a -> Tree a
unionSet a b = listToTree $ S.unionSet as bs where
    as = treeToList' a
    bs = treeToList' b

-- |
-- >>> intersectionSet (Branch 2 (Leaf 1) Empty) (Branch 3 (Leaf 2) (Leaf 4))
-- Leaf 2
intersectionSet :: Ord a => Tree a -> Tree a -> Tree a
intersectionSet a b = listToTree $ S.intersectionSet as bs where
    as = treeToList' a
    bs = treeToList' b
