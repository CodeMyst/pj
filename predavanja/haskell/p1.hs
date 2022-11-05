fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

duplicate :: Int -> Int
duplicate x = 2 * x

duplicateEven x = if even x then 2 * x else x

max2 a b = if a > b then a else b
max3 :: Int -> Int -> Int -> Int
max3 a b = max2 (max2 a b)

fact 0 = 1
fact n = n * fact (n - 1)

pow :: Int -> Int -> Int
pow _ 0 = 1
pow a 1 = a
pow a k = a * pow a (k - 1)

factAcc 0 acc = acc
factAcc n acc = factAcc (n - 1) (n * acc)
fact' n = factAcc n 1

fibAcc f1 f2 cnt n =
    if n == cnt
        then f2
        else fibAcc f2 (f1 + f2) (cnt + 1) n
fib' 1 = 1
fib' 2 = 2
fib' n = fibAcc 1 1 2 n

sign n
    | n < 0 = "negative"
    | n == 0 = "zero"
    | otherwise = "positive"

areaOfCylinder r h =
    let
        base = r * r * 3.14
        wrapper = 2 * r * 3.14 * h
    in 2 * base + wrapper

prime 2 = True
prime x =
    let
        boundary = (round . sqrt . fromIntegral) x
        noDivisor d num
            | num > boundary   = True
            | d `mod` num == 0 = False
            | otherwise        = noDivisor d (num + 1)
    in noDivisor x 2

bodyMassIndex weightKg heightMeters
    | bmi <= skinny = "low"
    | bmi <= normal = "normal"
    | bmi <= fat    = "high"
    | otherwise     = "very high"
    where bmi = weightKg / heightMeters ^ 2
          skinny = 18.5
          normal = 25.0
          fat    = 30.0

fibCase k =
    case k of
        1 -> 1
        2 -> 1
        _ -> fibCase (k - 1) + fibCase (k - 2)

(\+) :: Bool -> Bool -> Bool
(\+) False b = b
(\+) True _ = True

mul3 = \x y z -> x * y * z

_max :: (Ord t) => t -> t -> t
_max a b = if a > b then a else b

maxPlus5 :: (Num t, Ord t) => t -> t -> t
maxPlus5 a b = _max a b + 5

class Negation a where
    neg :: a -> a

instance Negation Integer where
    neg k = k * (-1)

instance Negation Bool where
    neg True  = False
    neg False = True
