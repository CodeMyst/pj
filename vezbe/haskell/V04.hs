-- split string

split :: Char -> String -> [String]
split _ [] = [""]
split c (x:xs)
    | c == x    = "" : rest
    | otherwise = (x : head rest) : tail rest
    where
        rest = split c xs

-- string join

join :: [String] -> String
join [] = []
join [x] = x
join (x:xs) = x ++ "," ++ join xs

-- split by ' ' and join, map fold

splitJoin :: [String] -> String
-- splitJoin s = join $ foldl (++) [] $ map (split ' ') s
splitJoin s = join $ concatMap (split ' ') s

-- [[Int]], square all ints, sum them, return product

intMadness :: [[Int]] -> Int
intMadness s = product $ map (sum . map (^2)) s

-- Definisati tip podataka naselje. Naselje moze da bude Selo, Varosica ili Grad
-- Sva 3 tipa naselja mogu imati broj stanovnika (integer) i povrsinu (double)
-- Selo nosi informaciju o tome da li je "zbijeno" ili "razbijeno" (string)
-- Grad ima dodatni parametar koji kaze da li sadrzi gradski bazen ili ne (boolean).

data Naselje =
    Selo {
        populacija :: Int,
        povrsina   :: Double,
        zbijeno    :: String
    } |
    Varosica {
        populacija :: Int,
        povrsina   :: Double
    } |
    Grad {
        populacija :: Int,
        povrsina   :: Double,
        imaBazen   :: Bool
    } deriving Show

-- Napisati funkciju koja iz liste Naselja izdvaja sva razbijena sela i sve
-- gradove sa bazenima koji imaju vise od 150 000 stanovnika.

izdvoj :: [Naselje] -> [Naselje]
izdvoj [] = []
izdvoj l = [x | x <- l, (jeSelo x && zbijeno x == "razbijeno") || (jeGrad x && imaBazen x && populacija x >= 150000)]
    where
        jeSelo (Selo {}) = True
        jeSelo _         = False
        jeGrad (Grad {}) = True
        jeGrad _         = False

