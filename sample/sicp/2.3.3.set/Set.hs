module Set 
( elementOfSet
, intersectionSet
, unionSet
, adjoinSet
) where

elementOfSet :: Ord a => a -> [a] -> Bool
elementOfSet _ [] = False
elementOfSet x (s:ss)
    | x == s    = True
    | x < s     = False
    | otherwise = elementOfSet x ss
    
intersectionSet :: Ord a => [a] -> [a] -> [a]
intersectionSet [] _ = []
intersectionSet _ [] = []
intersectionSet set1@(l:ls) set2@(r:rs)
    | l == r = l : intersectionSet ls rs
    | l <  r = intersectionSet ls set2
    | r <  l = intersectionSet set1 rs

adjoinSet :: Ord a => a -> [a] -> [a]
adjoinSet x [] = [x]
adjoinSet x set@(s:ss)
    | x == s    = set
    | x <  s    = x : set
    | otherwise = s : adjoinSet x ss

unionSet :: Ord a => [a] -> [a] -> [a]
unionSet [] rs = rs
unionSet ls [] = ls
unionSet set1@(l:ls) set2@(r:rs)
    | l == r    = l : unionSet ls rs 
    | l <  r    = l : unionSet ls set2
    | l >  r    = r : unionSet set1 rs
