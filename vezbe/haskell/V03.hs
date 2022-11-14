-- data type, either empty or Node which holds info and the reference to the next element

data Element a = Empty | Node a (Element a) deriving Show

-- from a list of ints create a new list using the custom type

createMyList :: [Int] -> Element Int
createMyList [] = Empty
createMyList (x:xs) = Node x (createMyList xs)

-- length of the custom list

myLength :: Element a -> Int
myLength Empty = 0
myLength (Node x xs) = 1 + myLength xs

-- is element in list

contains :: Eq a => a -> Element a -> Bool
contains _ Empty = False
contains a (Node x xs) = if a == x then True else contains a xs

-- data type Planet, Empty or (name, radius, gaseous)

data Planet = EmptyPlanet | Planet {
    name    :: String,
    radius  :: Double,
    gaseous :: Bool
} deriving Show

-- planets list type

type Planets = Element Planet

-- find planet by name

findPlanetByName :: String -> Planets -> Planet
findPlanetByName _ Empty = EmptyPlanet
findPlanetByName s (Node x xs) = if name x == s then x else findPlanetByName s xs

-- return gaseous planets

findGaseousPlanets :: Planets -> Planets
findGaseousPlanets Empty = Empty
findGaseousPlanets (Node x xs)
    | gaseous x = Node x (findGaseousPlanets xs)
    | otherwise = findGaseousPlanets xs

