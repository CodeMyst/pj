-- remove last element from list

removeLast :: [a] -> [a]
removeLast [] = []
removeLast [x] = []
removeLast (x:xs) = x : removeLast xs

-- remove second to last element

removeSecondToLast :: [a] -> [a]
removeSecondToLast [] = []
removeSecondToLast [_, x] = [x]
removeSecondToLast (x:xs) = x : removeSecondToLast xs

-- n! recursive

fact :: Int -> Int
fact 0 = 1
fact 1 = 1
fact n = n * fact (n - 1)

-- n! tail-recursive

fact' n = factTail n 1
factTail n acc
    | n == 0    = acc
    | otherwise = factTail (n - 1) (n * acc)

-- check if string has capital letters

hasCapitalLetters :: String -> Bool
hasCapitalLetters [] = False
hasCapitalLetters (x:xs)
    | x >= 'A' && x <= 'Z' = True
    | otherwise          = hasCapitalLetters xs

-- flatten

flatten :: Eq a => [a] -> [a]
flatten [] = []
flatten [x] = [x]
flatten (x:y:xs)
    | x == y    = flatten (x : xs)
    | otherwise = x : flatten (y : xs)

-- from a list of strings, remove strings which have all lowercase letters

removeLowercaseStrings :: [String] -> [String]
removeLowercaseStrings [] = []
removeLowercaseStrings (x:xs)
    | hasCapitalLetters x = x : removeLowercaseStrings xs
    | otherwise           = removeLowercaseStrings xs

-- square all elements in list (list comprehensions)

squareAll :: Num a => [a] -> [a]
squareAll x = [c ^ 2 | c <- x]

-- isDivisible, check if first number can divide second

isDivisible :: Int -> Int -> Bool
isDivisible a b = b `mod` a == 0

-- isDivisibleBy3

isDivisibleBy3 :: Int -> Bool
isDivisibleBy3 = isDivisible 3

-- filter list l using func f

filter' :: (a -> Bool) -> [a] -> [a]
filter' f l = [c | c <- l, f c]

-- divisible by 3

divisibleBy3 :: [Int] -> [Int]
divisibleBy3 = filter' isDivisibleBy3
