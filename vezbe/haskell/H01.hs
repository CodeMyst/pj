-- remove every n-th element

removeNth :: [a] -> Int -> [a]
removeNth l n = removeNthHelp l n 0

removeNthHelp :: [a] -> Int -> Int -> [a]
removeNthHelp [] _ _ = []
removeNthHelp (x:xs) n k
    | n == k + 1 = removeNthHelp xs n 0
    | otherwise  = x : removeNthHelp xs n (k + 1)

-- return a list of divisors of a positive number

divisors :: Int -> [Int]
divisors 0 = []
divisors n = [x | x <- [1..n], mod n x == 0]

-- quicksort

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort (smaller xs) ++ [x] ++ quicksort (bigger xs)
    where
        bigger  = filter (> x)
        smaller = filter (<= x)

-- filter using list comprehensions

filter' :: (a -> Bool) -> [a] -> [a]
filter' f l = [c | c <- l, f c]

-- list sum

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum xs

-- zip - add 2 lists

zip' :: Num a => [a] -> [a] -> [a]
zip' [] [] = []
zip' l1 [] = l1
zip' [] l2 = l2
zip' (x:xs) (y:ys) = (x + y) : zip' xs ys

-- sum all digits of a number, recursive

digitSum :: Int -> Int
digitSum 0 = 0
digitSum n = mod n 10 + digitSum (div n 10)

-- sum all digits of a number, tail recursive

digitSum' :: Int -> Int
digitSum' n = digitSumAcc n 0

digitSumAcc :: Int -> Int -> Int
digitSumAcc 0 acc = acc
digitSumAcc n acc = digitSumAcc (div n 10) (acc + mod n 10)

-- sum even digits of a number

evenDigitSum :: Int -> Int
evenDigitSum 0 = 0
evenDigitSum n
    | mod (mod n 10) 2 == 0 = mod n 10 + evenDigitSum (div n 10)
    | otherwise             = evenDigitSum (div n 10)

