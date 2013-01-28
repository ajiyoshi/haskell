-- SICP 2.3.4 Huffman符号化木
module Huffman
( CodeTree
, symbol
, weight
, decode
, encode
, generateHuffmanTree
, sampleTree , sampleTree2 , sampleMessage2
, left , right
) where

import Data.Char

type Symbol = String
type Weight = Int

data CodeTree = CodeTree CodeTree CodeTree | Leaf Symbol Weight | Empty deriving(Show, Read)

left :: CodeTree -> CodeTree
left (CodeTree l _) = l
left _ = Empty

right :: CodeTree -> CodeTree
right (CodeTree _ r) = r
right _ = Empty

-- |
-- >>> symbol (Leaf "hoge" 1)
-- ["hoge"]
--
-- >>> symbol (CodeTree (Leaf "a" 2) (Leaf "b" 3))
-- ["a","b"]
--
-- >>> symbol (CodeTree (CodeTree (Leaf "c" 3) (Leaf "d" 4)) (Leaf "b" 3))
-- ["c","d","b"]
symbol :: CodeTree -> [Symbol]
symbol (Leaf s _) = [s]
symbol (CodeTree l r) = symbol l ++ symbol r
symbol Empty = []

-- |
-- >>> weight (Leaf "hoge" 1)
-- 1
--
-- >>> weight (CodeTree (Leaf "a" 2) (Leaf "b" 3))
-- 5
--
-- >>> weight (CodeTree (CodeTree (Leaf "c" 3) (Leaf "d" 4)) (Leaf "b" 3))
-- 10
weight :: CodeTree -> Weight
weight (Leaf _ w) = w
weight (CodeTree l r) = weight l + weight r
weight Empty = 0

type Bits = [Int]

decode :: Bits -> CodeTree -> [Symbol]
decode [] _ = []
decode bits root = itr bits root where
    itr [] _ = []
    itr (b:bs) current = ret $ chooseBranch b current where
        ret (Leaf s _) = s : itr bs root
        ret next = itr bs next

-- |
-- >>> chooseBranch 0 (CodeTree (Leaf "left" 1) (Leaf "right" 2))
-- Leaf "left" 1
--
-- >>> chooseBranch 1 (CodeTree (Leaf "left" 1) (Leaf "right" 2))
-- Leaf "right" 2
chooseBranch :: Int -> CodeTree -> CodeTree
chooseBranch 0 = left
chooseBranch 1 = right

{-
adjoinSet :: CodeTree -> [CodeTree] -> [CodeTree]
adjoinSet x [] = [x]
adjoinSet x set@(t:ts)
    | weight x < weight t = x : set
    | otherwise = t : adjoinSet x ts

makeLeafSet :: [Leaf] -> 
-}

-- |
-- >>> decode [0,1,1,0,0,1,0,1,0,1,1,1,0] sampleTree
-- ["A","D","A","B","B","C","A"]
sampleTree :: CodeTree
sampleTree = CodeTree (Leaf "A" 4) (CodeTree (Leaf "B" 4) (CodeTree (Leaf "D" 1) (Leaf "C" 1)))
                        
-- |
-- >>> encode ["A","D","A","B","B","C","A"] sampleTree
-- [0,1,1,0,0,1,0,1,0,1,1,1,0]
encode :: [Symbol] -> CodeTree -> Bits
encode [] _ = []
encode (s:ss) tree = encodeSymbol s tree ++ encode ss tree

encodeSymbol :: Symbol -> CodeTree -> Bits
encodeSymbol _ (Leaf _ _) = []
encodeSymbol s (CodeTree l r)
    | s `elem` symbol l = 0 : encodeSymbol s l
    | s `elem` symbol r = 1 : encodeSymbol s r

-- |
-- >>> :m + Data.List
-- >>> let wc = map (\l -> (head l, length l)) . group . sort . words 
-- >>> let ls = wc "a a a a a a a a b b b c d e f g g"
-- >>> let tree = generateHuffmanTree ls 
--
-- >>> symbol tree
-- ["a","g","f","e","d","c","b"]
--
-- >>> symbol . left $ tree
-- ["a"]
--
-- >>> symbol . right $ tree
-- ["g","f","e","d","c","b"]
--
-- >>> weight tree
-- 17
generateHuffmanTree :: [(Symbol, Int)] -> CodeTree
generateHuffmanTree = succesiveMerge . makeLeafSet

succesiveMerge :: [CodeTree] -> CodeTree
succesiveMerge (a:b:c) = succesiveMerge $ adjoinSet (CodeTree a b) c
succesiveMerge (a:[]) = a

-- |
-- >>> makeLeafSet [("A", 4), ("B", 2), ("C", 1), ("D", 1)]
-- [Leaf "D" 1,Leaf "C" 1,Leaf "B" 2,Leaf "A" 4]
makeLeafSet :: [(Symbol, Int)] -> [CodeTree]
makeLeafSet [] = []
makeLeafSet ((s, w):es) = adjoinSet (Leaf s w) (makeLeafSet es)

adjoinSet :: CodeTree -> [CodeTree] -> [CodeTree]
adjoinSet x [] = [x]
adjoinSet x set@(t:ts)
    | weight x < weight t = x : set
    | otherwise = t : adjoinSet x ts

-- |
-- >>> sampleTree2
-- CodeTree (Leaf "NA" 16) (CodeTree (Leaf "YIP" 9) (CodeTree (CodeTree (Leaf "A" 2) (CodeTree (Leaf "WAH" 1) (Leaf "BOOM" 1))) (CodeTree (Leaf "SHA" 3) (CodeTree (Leaf "JOB" 2) (Leaf "GET" 2)))))
sampleTree2 :: CodeTree
sampleTree2 = generateHuffmanTree
    [("A", 2), ("NA", 16), ("BOOM", 1), ("SHA", 3), ("GET", 2), ("YIP", 9), ("JOB", 2), ("WAH", 1)]

-- |
-- >>> let enc =  encode sampleMessage2 sampleTree2 
-- >>> length enc
-- 84
-- >>> sampleMessage2 == decode enc sampleTree2
-- True
sampleMessage2 :: [Symbol]
sampleMessage2 = words . map toUpper $ "Get a job "
    ++ "Sha na na na na na na na na "
    ++ "Get a job "
    ++ "Sha na na na na na na na na "
    ++ "Wah yip yip yip yip yip yip yip yip yip "
    ++ "Sha boom"

