import Data.Char
import Data.Numbers.Primes (isPrime)

-- filter digits

digits :: String -> String
digits = filter isDigit

-- filter strings >= 5 && <= 15, and filter digits

between :: Int -> Int -> Int -> Bool
between x min max
    | x >= min  = x <= max
    | otherwise = False

filterLen :: [String] -> [String]
filterLen s = filter (\x -> between (length x) 5 15) (map digits s)

-- remove spaces, convert to lower

removeSpaceToLower :: String -> String
removeSpaceToLower s = map toLower (filter (/= ' ') s)

-- check if palindrome, use previous function

palindrome :: String -> Bool
palindrome s = removeSpaceToLower s == reverse (removeSpaceToLower s)

-- check if all strings are a palindrome

palindromeList :: [String] -> Bool
palindromeList = all palindrome

-- filter prime nums

primes :: [Int] -> [Int]
primes = filter isPrime

-- use mod on numbers, filter primes

remainder :: [Int] -> [Int]
remainder l = map (`mod` length l) (primes l)

-- [[Int]], if length >= 5 then sum, otherwise product

sumOrProd :: [[Int]] -> [Int]
sumOrProd = map (\x -> if length x >= 5 then sum x else product x)

