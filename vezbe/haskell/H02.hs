import Data.Char
import V02

-- if list has even number of elements square all elements, otherwise multiply by 10

squareOrMul :: [Int] -> [Int]
squareOrMul l
    | even (length l) = map (^ 2) l
    | otherwise       = map (* 10) l

-- remove all uppercase letters, convert the rest to uppercase

onlyUpper :: String -> String
onlyUpper s = map toUpper $ filter isLower s

-- apply function with 3 args
-- 1. f1 :: [[Int]] -> [[Int]]
-- 2. f2 :: [[Int]] -> [Int]
-- 3. [[Int]]
--
-- apply f1 and f2 on the passed list of lists

apply :: ([[Int]] -> [[Int]]) -> ([[Int]] -> [Int]) -> [[Int]] -> [Int]
apply f1 f2 l = f2 $ f1 l

-- removeEvenAndSum using the apply, sumLists and removeEven functions

removeEvenAndSum :: [[Int]] -> [Int]
removeEvenAndSum = apply removeEven sumLists

-- average length of a list of strings using fold and map

averageLength :: [String] -> Int
averageLength l = (foldl (+) (0) (map length l)) `div` (length l)

