{-
    Zadatak 1 (10p)

    Potrebno je napisati funkciju koja prihvata listu listi integer-a [[Int]]. Svakom elementu
    na parnom mestu u svakoj podlisti promeni znak. (Indeksiranje pocinje od 0)

    Zatim, napisati funkciju koja prima [[Int]] i nad njom primeni prethodnu funkciju.
    Nakon sumira samo parne elemente svake podliste (map i fold). Posle toga izbacuju
    se sve sume koje su manje od 99 (filter).
-}

promeniZnak :: [[Int]] -> [[Int]]
promeniZnak = map(\l -> zipWith (*) l (cycle [-1, 1]))

sumiraj :: [[Int]] -> [Int]
sumiraj xs = filter (>= 99) (map(foldl (\a x -> if even x then a + x else a) 0) (promeniZnak xs))

{-
    Zadatak 2 (8p)

    Definisati tip podataka StambeniObjekat. Stambeni objekat opisan je svojom
    kvadraturom (double) i kapacitetom (int - broj ljudi koji moze da zivi u tom objektu).
    StambeniObjekat moze biti SeoskaKuca, GradskaKuca i Zgrada. SeoskaKuca kao
    dodatnu osobinu ima povrsinu dvorista u arima (double), a zgrada ima broj spratova
    (int).

    Napisati funkciju koja iz liste stambenih objekata izdvaja one SeoskeKuce koje imaju
    dvoriste od barem 5 ari, sve gradske kuce, kao i zgrade koje su visoke barem 7
    spratova. Na kraju potrebno je vratiti ukupni kapacitet svih izdvojenih objekata.
-}

data StambeniObjekat =
    SeoskaKuca {
        kvadratura :: Double,
        kapacitet :: Int,
        povrsinaDvorista :: Double
    } |
    GradskaKuca {
        kvadratura :: Double,
        kapacitet :: Int
    } |
    Zgrada {
        kvadratura :: Double,
        kapacitet :: Int,
        brojSpratova :: Int
    } deriving Show

testZgrade = [GradskaKuca 112 5, GradskaKuca 165 8, Zgrada 1543 35 6, Zgrada 2054 56 8, SeoskaKuca 256 10 4, SeoskaKuca 415 12 7]

izdvoji :: [StambeniObjekat] -> [StambeniObjekat]
izdvoji = filter odgovara
    where
        odgovara (GradskaKuca _ _) = True
        odgovara (SeoskaKuca _ _ dvoriste) = dvoriste >= 5
        odgovara (Zgrada _ _ spratova) = spratova >= 7

ukupniKapacitet :: [StambeniObjekat] -> Int
ukupniKapacitet l = sum $ map kapacitet l

{-
    Zadatak 3 (7p)

    Potrebno je napisati funkciju koja ??e da izvr??i klasterizovanje (grupisanje) liste ta??aka L
    u K klastera (grupa) pomo??u K-Means algoritma. Ovaj algoritam se izvodi u vi??e
    iteracija. Po??ev od nekih K centralnih ta??aka (centri klastera), algoritam izvodi slede??e
    korake:

    - svakoj ta??ki iz L prona??e najbli??u od centralnih ta??aka. Ta??ka pripada onom
    klasteru ??ijem centru je najbli??a.
    - nakon ??to je svakoj ta??ki iz L dodeljen klaster, ra??unaju se nove centralne ta??ke.

    Novi centar svakog klastera ra??una se na osnovu svih ta??aka iz L koji pripadaju
    tom klasteru. Centar klastera i u narednoj iteraciji ra??una se kao suma svih ta??aka klastera i
    u trenutnoj iteraciji podeljena sa brojem ta??aka koji pripadaju tom klasteru.

    Algoritam se zavr??ava kada izme??u 2 iteracije ne do??e do promene u klasterima. Za
    po??etne centre klastera uzeti K nasumi??nih ta??aka iz liste ta??aka L.
    Glavna funkcija o??ekuje listu ta??aka L (ta??ka = par Double-ova), kao i neko K koje
    predstavlja broj klastera na koji je potrebno podeliti listu ta??aka L. Funkcija vra??a listu
    parova oblika:
    ((Double, Double), Int) koji predstavljaju (tacku, broj klastera)
-}

type Tacka = (Double, Double)

-- klasteruj :: [Tacka] -> Int -> ((Double, Double), Int)

