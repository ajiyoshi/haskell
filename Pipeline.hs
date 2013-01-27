module Pipeline
( (|>)
, (|>>)
) where

-- |pipeline operator
-- >>> [1 .. 10] |> map (+1) |> filter even |> sum |> show |> putStr
-- 30
--
-- >>> :m + Data.List
-- >>> "ab a b a a b" |> words |> sort |> group |> map (\ls -> (head ls, length ls))
-- [("a",3),("ab",1),("b",2)]
--
-- >>> :m + Data.List
-- >>> map (\ls -> (head ls, length ls)) . group . sort . words $ "ab a b a a b"
-- [("a",3),("ab",1),("b",2)]
--
-- >>> :m + Data.List
-- >>> let wc s = s |> words |> sort |> group |> map (\ls -> (head ls, length ls)) in wc "a b a"
-- [("a",2),("b",1)]
infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

-- |pipelined compose operator
-- >>> (map (+1) |>> filter even |>> sum |>> show |>> putStr) [1 .. 10]
-- 30
--
-- >>> :m + Data.List
-- >>> ( words |>> sort |>> group |>> map (\ls -> (head ls, length ls)) ) "ab a b a a b"
-- [("a",3),("ab",1),("b",2)]
infixl 0 |>>
(|>>) :: (a -> b) -> (b -> c) -> a -> c
g |>> f = f . g
