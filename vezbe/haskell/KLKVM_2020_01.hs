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

    Potrebno je napisati funkciju koja će da izvrši klasterizovanje (grupisanje) liste tačaka L
    u K klastera (grupa) pomoću K-Means algoritma. Ovaj algoritam se izvodi u više
    iteracija. Počev od nekih K centralnih tačaka (centri klastera), algoritam izvodi sledeće
    korake:

    - svakoj tački iz L pronađe najbližu od centralnih tačaka. Tačka pripada onom
    klasteru čijem centru je najbliža.
    - nakon što je svakoj tački iz L dodeljen klaster, računaju se nove centralne tačke.

    Novi centar svakog klastera računa se na osnovu svih tačaka iz L koji pripadaju
    tom klasteru. Centar klastera i u narednoj iteraciji računa se kao suma svih tačaka klastera i
    u trenutnoj iteraciji podeljena sa brojem tačaka koji pripadaju tom klasteru.

    Algoritam se završava kada između 2 iteracije ne dođe do promene u klasterima. Za
    početne centre klastera uzeti K nasumičnih tačaka iz liste tačaka L.
    Glavna funkcija očekuje listu tačaka L (tačka = par Double-ova), kao i neko K koje
    predstavlja broj klastera na koji je potrebno podeliti listu tačaka L. Funkcija vraća listu
    parova oblika:
    ((Double, Double), Int) koji predstavljaju (tacku, broj klastera)
-}

type Tacka = (Double, Double)

-- klasteruj :: [Tacka] -> Int -> ((Double, Double), Int)

