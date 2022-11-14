module V02 where

-- flatten an int list of lists

flatten :: [[Int]] -> [Int]
flatten = foldl (++) []

-- sum int lists inside an int list using the fold func

sumLists :: [[Int]] -> [Int]
sumLists = foldl (\acc x -> acc ++ [sum x]) []

-- remove all even numbers from [[Int]] using filter
-- remove entire list if it ends up empty

removeEven :: [[Int]] -> [[Int]]
removeEven l = filter (not . null) $ map (filter odd) l

-- reverse all strings in a list using map and reverse

reverseAll :: [String] -> [String]
reverseAll = map reverse

-- remove all numbers divisible by 3 from [[Int]]

removeDivBy3 :: [[Int]] -> [[Int]]
removeDivBy3 l = filter (not . null) $ map (filter notDivisibleBy3) l
    where
        notDivisibleBy3 x = x `mod` 3 /= 0

-- use the previous function, remove all lists with less than 5 elements

removeDivBy3' :: [[Int]] -> [[Int]]
removeDivBy3' l = filter (\x -> length x >= 5) (removeDivBy3 l)

