-- tree type, either empty or a node which has an Int and a left and right tree

data Tree = Empty | Node Int Tree Tree deriving Show

-- check if the tree contains the element

contains :: Int -> Tree -> Bool
contains _ Empty = False
contains a (Node x l r)
    | a == x = True
    | otherwise = contains a l || contains a r

-- toList, convert tree to list

toList :: Tree -> [Int]
toList Empty = []
toList (Node x l r) = x : toList l ++ toList r

-- toList all elements divisible by 3 or 5

divisibleBy3Or5 :: Tree -> [Int]
divisibleBy3Or5 Empty = []
divisibleBy3Or5 (Node x l r) = [x | x `mod` 3 == 0 || x `mod` 5 == 0] ++ divisibleBy3Or5 l ++ divisibleBy3Or5 r

-- mirror tree

mirror :: Tree -> Tree
mirror Empty = Empty
mirror (Node x l r) = Node x (mirror r) (mirror l)

-- filter tree to list

filterTree :: (Int -> Bool) -> Tree -> [Int]
filterTree _ Empty = []
filterTree f (Node x l r) = [x | f x] ++ filterTree f l ++ filterTree f r

