module Set 
( elementOfSet
, intersectionSet
, unionSet
, adjoinSet
) where

-- |
-- >>> elementOfSet 1 []
-- False
--
-- >>> elementOfSet 2 [1, 2, 3]
-- True
elementOfSet :: Ord a => a -> [a] -> Bool
elementOfSet _ [] = False
elementOfSet x (s:ss)
    | x == s    = True
    | x < s     = False
    | otherwise = elementOfSet x ss
    
-- |
-- >>> intersectionSet [1,3,5] [1,2,3]
-- [1,3]
intersectionSet :: Ord a => [a] -> [a] -> [a]
intersectionSet [] _ = []
intersectionSet _ [] = []
intersectionSet set1@(l:ls) set2@(r:rs)
    | l == r = l : intersectionSet ls rs
    | l <  r = intersectionSet ls set2
    | r <  l = intersectionSet set1 rs

-- |
-- >>> adjoinSet 1 []
-- [1]
--
-- >>> adjoinSet 3 . adjoinSet 1 $ []
-- [1,3]
--
-- >>> adjoinSet 2 . adjoinSet 3 . adjoinSet 1 $ []
-- [1,2,3]
--
-- >>> adjoinSet 1 . adjoinSet 1 $ []
-- [1]
adjoinSet :: Ord a => a -> [a] -> [a]
adjoinSet x [] = [x]
adjoinSet x set@(s:ss)
    | x == s    = set
    | x <  s    = x : set
    | otherwise = s : adjoinSet x ss

-- |
-- >>> unionSet [1,3,5] [1,2,3]
-- [1,2,3,5]
unionSet :: Ord a => [a] -> [a] -> [a]
unionSet [] rs = rs
unionSet ls [] = ls
unionSet set1@(l:ls) set2@(r:rs)
    | l == r    = l : unionSet ls rs 
    | l <  r    = l : unionSet ls set2
    | l >  r    = r : unionSet set1 rs
