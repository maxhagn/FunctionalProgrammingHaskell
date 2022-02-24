module Angabe5 where

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
-}

import Data.List
import Data.Char

type Nat0 = Int


-- Die selbstdefinierte Typklasse Menge_von:

class Eq a => Menge_von a where
 leer          :: [] a
 vereinige     :: [] a -> [] a -> [] a
 schneide      :: [] a -> [] a -> [] a
 ziehe_ab      :: [] a -> [] a -> [] a
 ist_teilmenge :: [] a -> [] a -> Bool
 ist_obermenge :: [] a -> [] a -> Bool
 ist_element   :: a -> [] a -> Bool
 ist_leer      :: [] a -> Bool
 sind_gleich   :: [] a -> [] a -> Bool
 anzahl        :: a -> [] a -> Nat0

 -- Protoimplementierungen
 leer = []
 vereinige xs ys     = xs ++ ys
 ist_teilmenge xs ys = ist_obermenge ys xs
 ist_obermenge xs ys = ist_teilmenge ys xs
 ist_element x xs    = anzahl x xs >= 1
 ist_leer xs         = xs == leer
 sind_gleich xs ys   = ist_teilmenge xs ys && ist_teilmenge ys xs


-- Weitere Typen:

newtype Paar a b = P (a,b) deriving (Eq,Ord,Show)

data Zahlraum_0_10 = N | I | II | III | IV | V | VI
                     | VII | VIII | IX | X | F deriving (Eq,Ord,Show)

newtype Funktion = Fkt { f :: Zahlraum_0_10 -> Zahlraum_0_10 }

data Baum a = Blatt a | Knoten (Baum a) a (Baum a) deriving (Eq,Ord,Show)

newtype ElemTyp a = ET a

-- Pseudoheterogene Elementtypen
data PH_ElemTyp a b c d e = A a | B b | C c | D d | E e deriving (Eq,Ord,Show)
data PH_ElemTyp' q r s    = Q q | R r | S s deriving (Eq,Ord,Show)




-- Aufgabe A.1

wert :: Zahlraum_0_10 -> Nat0
wert z | z == N = 0
       | z == I = 1
       | z == II = 2
       | z == III = 3
       | z == IV = 4
       | z == V = 5
       | z == VI = 6
       | z == VII = 7
       | z == VIII = 8
       | z == IX = 9
       | z == X = 10
       | otherwise = 99

zuZahlraum_0_10 :: Nat0 -> Zahlraum_0_10
zuZahlraum_0_10 z | z == 0 = N
                  | z == 1 = I
                  | z == 2 = II
                  | z == 3 = III
                  | z == 4 = IV
                  | z == 5 = V
                  | z == 6 = VI
                  | z == 7 = VII
                  | z == 8 = VIII
                  | z == 9 = IX
                  | z == 10 = X
                  | otherwise = F

instance Num Zahlraum_0_10 where
 z1 + z2       = zuZahlraum_0_10(wert z1 + wert z2)
 z1 - z2       = zuZahlraum_0_10(wert z1 - wert z2)
 z1 * z2       = zuZahlraum_0_10(wert z1 * wert z2)
 abs z1        = z1
 signum z1     = I
 fromInteger i = zuZahlraum_0_10 (fromIntegral i)

{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer Num folgendermassen vor:
   Die Num Instanz von Zahlraum_0_10 wandelt die einzelenen Röm Zahlen zu verwendbaren
   lat Zahlen um und berechnet das ergebnis.
   Im anschluss wird das Ergebnis wieder in einen Wert des Types Zahlraum_0_10 umgewandelt,
   wobei ungültige Werte hier direkt zu F werden.
-}




-- Aufgabe A.2
data Bild = Bi [(Zahlraum_0_10,Zahlraum_0_10)] deriving (Eq,Ord)

instance Show Bild where
 show (Bi b) = show b

runFunction :: (Zahlraum_0_10 -> Zahlraum_0_10) -> Bild
runFunction f = Bi [ (n, f n) | n <- head (tails [N, I, II, III, IV, V, VI, VII, VIII, IX, X, F]) ]

instance Eq Funktion where
 Fkt f1 == Fkt f2 = runFunction f1 == runFunction f2
 Fkt f1 /= Fkt f2 = runFunction f1 /= runFunction f2

instance Show Funktion where
 show (Fkt f) = "{" ++ tail (init (show (runFunction f))) ++ "}"

{- Knapp, aber gut nachvollziehbar gehen die beiden Instanzbildungen fuer
   Hierfür wurde eine Hilfsklasse runFunction geschrieben die das Ergebnis einer
   Funktion als Bild darstellt. Dann wird das Bild einfach verglichen bzw in richtiger Form
   angezeigt.
-}


-- Aufgabe A.3

duplikatFrei :: (Ord a) => [a] -> Bool
duplikatFrei xs = length (nub xs) == length xs

count :: Eq a => a -> [a] -> Nat0
count x = length . filter (x==)

runFunctions :: [(Zahlraum_0_10 -> Zahlraum_0_10)] -> [Bild]
runFunctions f = [Bi [ (n, m n) | n <- head (tails [N, I, II, III, IV, V, VI, VII, VIII, IX, X, F]) ] | m <- head (tails f)]

instance Ord Funktion where
  (Fkt f1) `compare` (Fkt f2) = runFunction f1 `compare` runFunction f2

instance Menge_von Int where
 leer                = []
 vereinige xs ys     | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                     | otherwise = xs ++ ys
 schneide xs ys      | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                     | otherwise = let ns = [ x | x <- xs, elem x ys] in [ y | y <- ys, elem y ns]
 ziehe_ab xs ys      | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                     | otherwise = xs \\ ys
 ist_teilmenge xs ys | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                     | otherwise = sum [ 1 | i <- head (tails xs), ist_element i ys] == length xs
 ist_obermenge xs ys | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                     | otherwise = ist_teilmenge ys xs
 ist_element x xs    | not (duplikatFrei xs) = error "Fehler"
                     | otherwise = count x xs >= 1
 ist_leer xs         | not (duplikatFrei xs) = error "Fehler"
                     | otherwise = xs == leer
 sind_gleich xs ys   | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                     | otherwise = ist_teilmenge xs ys && ist_teilmenge ys xs
 anzahl x xs         = count x xs


instance Menge_von Zahlraum_0_10 where
 leer                = []
 vereinige xs ys     | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                     | otherwise = xs ++ ys
 schneide xs ys      | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                     | otherwise = let ns = [ x | x <- xs, elem x ys] in [ y | y <- ys, elem y ns]
 ziehe_ab xs ys      | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                     | otherwise = xs \\ ys
 ist_teilmenge xs ys | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                     | otherwise = sum [ 1 | i <- head (tails xs), ist_element i ys] == length xs
 ist_obermenge xs ys | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                     | otherwise = ist_teilmenge ys xs
 ist_element x xs    | not (duplikatFrei xs) = error "Fehler"
                     | otherwise = count x xs >= 1
 ist_leer xs         | not (duplikatFrei xs) = error "Fehler"
                     | otherwise = xs == leer
 sind_gleich xs ys   | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                     | otherwise = ist_teilmenge xs ys && ist_teilmenge ys xs
 anzahl x xs         = count x xs

instance Menge_von Funktion where
 leer                            = []
 vereinige xs ys                 | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                                 | otherwise = xs ++ ys
 schneide xs ys                  | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                                 | otherwise = let ns = [ x | x <- xs, elem x ys] in [ y | y <- ys, elem y ns]
 ziehe_ab xs ys                  | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                                 | otherwise = xs \\ ys
 ist_teilmenge xs ys             | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                                 | otherwise = sum [ 1 | i <- head (tails xs), ist_element i ys] == length xs
 ist_obermenge xs ys             | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                                 | otherwise = ist_teilmenge ys xs
 ist_element x xs                | not (duplikatFrei xs) = error "Fehler"
                                 | otherwise = count x xs >= 1
 ist_leer xs                     | not (duplikatFrei xs) = error "Fehler"
                                 | otherwise = xs == leer
 sind_gleich xs ys               | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                                 | otherwise = ist_teilmenge xs ys && ist_teilmenge ys xs
 anzahl x xs                     = count x xs

{- Knapp, aber gut nachvollziehbar gehen die drei Instanzbildungen fuer
   Menge_von folgendermassen vor:
   Die Instanzbildungen gehen alle sehr ähnlich vor. Zu beachten ist die jeweilige prüfung auf eine
   gültige Menge. Ausserdem wurden Hilfsfunktionen wie etwa runFunctions benötigt um
   ein Array von Funktionen vergleichbar zu machen. Die einzelnen Operationen sind recht leicht verständlich.
-}




-- Aufgabe A.4

instance (Eq a, Ord a, Eq b, Ord b) => Menge_von (Paar a b) where
 leer                            = []
 vereinige xs ys                 | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                                 | otherwise = xs ++ ys
 schneide xs ys                  | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                                 | otherwise = let ns = [ x | x <- xs, elem x ys] in [ y | y <- ys, elem y ns]
 ziehe_ab xs ys                  | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                                 | otherwise = xs \\ ys
 ist_teilmenge xs ys             | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                                 | otherwise = sum [ 1 | i <- head (tails xs), ist_element i ys] == length xs
 ist_obermenge xs ys             | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                                 | otherwise = ist_teilmenge ys xs
 ist_element x xs                | not (duplikatFrei xs) = error "Fehler"
                                 | otherwise = count x xs >= 1
 ist_leer xs                     | not (duplikatFrei xs) = error "Fehler"
                                 | otherwise = xs == leer
 sind_gleich xs ys               | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                                 | otherwise = ist_teilmenge xs ys && ist_teilmenge ys xs
 anzahl x xs                     = count x xs

instance (Eq a, Ord a) => Menge_von (Baum a) where
 leer                            = []
 vereinige xs ys                 | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                                 | otherwise = xs ++ ys
 schneide xs ys                  | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                                 | otherwise = let ns = [ x | x <- xs, elem x ys] in [ y | y <- ys, elem y ns]
 ziehe_ab xs ys                  | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                                 | otherwise = xs \\ ys
 ist_teilmenge xs ys             | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                                 | otherwise = sum [ 1 | i <- head (tails xs), ist_element i ys] == length xs
 ist_obermenge xs ys             | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                                 | otherwise = ist_teilmenge ys xs
 ist_element x xs                | not (duplikatFrei xs) = error "Fehler"
                                 | otherwise = count x xs >= 1
 ist_leer xs                     | not (duplikatFrei xs) = error "Fehler"
                                 | otherwise = xs == leer
 sind_gleich xs ys               | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                                 | otherwise = ist_teilmenge xs ys && ist_teilmenge ys xs
 anzahl x xs                     = count x xs

{- Knapp, aber gut nachvollziehbar gehen die beiden Instanzbildungen fuer
   Menge_von folgendermassen vor:
   Die Instanzbildungen gehen sehr ähnlich denen aus A.3 vor. Zu beachten ist dass
   die einzelnen Typen Ord implementieren um die Operationen ausführen zu können.
-}



-- Aufgabe A.5

instance Eq a => Eq (ElemTyp a) where
 ET a1 == ET a2 = a1 == a2
 ET a1 /= ET a2 = a1 /= a2

instance Show a => Show (ElemTyp a) where
 show (ET a1) = show a1

{- Knapp, aber gut nachvollziehbar gehen die beiden Instanzbildungen fuer
   Eq und Show folgendermassen vor:
   Sehr selbsterklärend.
-}




-- Aufgabe A.6

instance (Eq a, Ord a) => Menge_von (ElemTyp a) where
 leer                 = []
 vereinige xs ys      = xs ++ ys
 schneide xs ys       = let ns = [ x | x <- xs, elem x ys] in [ y | y <- ys, elem y ns]
 ziehe_ab xs ys       = xs \\ ys
 ist_teilmenge xs ys  = sum [ 1 | i <- head (tails xs), ist_element i ys] == length xs
 ist_obermenge xs ys  = ist_teilmenge ys xs
 ist_element x xs     = count x xs >= 1
 ist_leer xs          = xs == leer
 sind_gleich xs ys    = ist_teilmenge xs ys && ist_teilmenge ys xs
 anzahl x xs          = count x xs

{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer
   Menge_von folgendermassen vor:
   Hier sollen MultiMengen Operationen möglich sein. Somit fällt für jede Operation der
   Check auf Menge weg (keine errors).
-}




-- Aufgabe A.7

instance (Eq a, Ord a, Eq b, Ord b, Eq c, Ord c, Eq d, Ord d, Eq e, Ord e) => Menge_von (PH_ElemTyp a b c d e) where
 leer                            = []
 vereinige xs ys                 | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                                 | otherwise = xs ++ ys
 schneide xs ys                  | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                                 | otherwise = let ns = [ x | x <- xs, elem x ys] in [ y | y <- ys, elem y ns]
 ziehe_ab xs ys                  | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                                 | otherwise = xs \\ ys
 ist_teilmenge xs ys             | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                                 | otherwise = sum [ 1 | i <- head (tails xs), ist_element i ys] == length xs
 ist_obermenge xs ys             | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                                 | otherwise = ist_teilmenge ys xs
 ist_element x xs                | not (duplikatFrei xs) = error "Fehler"
                                 | otherwise = count x xs >= 1
 ist_leer xs                     | not (duplikatFrei xs) = error "Fehler"
                                 | otherwise = xs == leer
 sind_gleich xs ys               | not (duplikatFrei xs) || not (duplikatFrei ys) = error "Fehler"
                                 | otherwise = ist_teilmenge xs ys && ist_teilmenge ys xs
 anzahl x xs                     = count x xs


{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer
   Menge_von folgendermassen vor:
   Sehr ähnlich A.4 (auf Ord achten!)
-}




-- Aufgabe A.8

instance (Eq p,Eq q,Eq r) => Menge_von (PH_ElemTyp' p q r) where
 leer                 = []
 vereinige xs ys      = xs ++ ys
 schneide xs ys       = let ns = [ x | x <- xs, elem x ys] in [ y | y <- ys, elem y ns]
 ziehe_ab xs ys       = xs \\ ys
 ist_teilmenge xs ys  = sum [ 1 | i <- head (tails xs), ist_element i ys] == length xs
 ist_obermenge xs ys  = ist_teilmenge ys xs
 ist_element x xs     = count x xs >= 1
 ist_leer xs          = xs == leer
 sind_gleich xs ys    = ist_teilmenge xs ys && ist_teilmenge ys xs
 anzahl x xs          = count x xs


{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer
   Menge_von folgendermassen vor:
   Sehr ähnlich A.6
-}


--Testdata
testAbb :: Zahlraum_0_10 -> Zahlraum_0_10
testAbb z = z + 2

testAbb2 :: Zahlraum_0_10 -> Zahlraum_0_10
testAbb2 z = z + 1

intArr1 :: [Int]
intArr1 = [1,2,3]

intArr2 :: [Int]
intArr2 = [3,5,6]

zrArr1 :: [Zahlraum_0_10]
zrArr1 = [I, II, III]

zrArr2 :: [Zahlraum_0_10]
zrArr2 = [IV, V, VI]

fArr1 :: [Funktion]
fArr1 = [(Fkt testAbb), (Fkt testAbb2)]

fArr2 :: [Funktion]
fArr2 = [(Fkt testAbb)]

pArr1 :: [Paar Int Zahlraum_0_10]
pArr1 = [(P (1,I)), (P (2,II))]

pArr2 :: [Paar Int Zahlraum_0_10]
pArr2 = [(P (1,I)), (P (2,II))]

bArr1 :: [Baum Int]
bArr1 = [(Knoten (Blatt 3) 4 (Knoten (Blatt 4) 2 (Blatt 3)))]

bArr2 :: [Baum Int]
bArr2 = [(Knoten (Blatt 3) 4 (Knoten (Blatt 4) 2 (Blatt 3)))]

main = do
    print("Aufgabe A.1")
    print(I + X)
    print(I + III)
    print(I - I)
    print(V - II)
    print(V - X)
    print(I * III)
    print(II * III)
    print(III * V)

    print("Aufgabe A.2")
    print(runFunction testAbb2)
    print((Fkt testAbb) == (Fkt testAbb))
    print((Fkt testAbb) == (Fkt testAbb2))
    print((Fkt testAbb))

    print("Aufgabe A.3")
    print(vereinige intArr1 intArr2)
    print(ist_teilmenge intArr1 intArr2)
    print(ist_obermenge intArr1 intArr2)
    print(ist_element 2 intArr1)
    print(ist_leer intArr1)
    print(sind_gleich intArr1 intArr2)
    print(schneide intArr1 intArr2)
    print(ziehe_ab intArr1 intArr2)

    print(vereinige zrArr1 zrArr2)
    print(ist_teilmenge zrArr1 zrArr2)
    print(ist_obermenge zrArr1 zrArr2)
    print(ist_element II zrArr1)
    print(ist_leer zrArr1)
    print(sind_gleich zrArr1 zrArr2)
    print(schneide zrArr1 zrArr2)
    print(ziehe_ab zrArr1 zrArr2)

    print(vereinige fArr1 fArr2)
    print(ist_teilmenge fArr1 fArr2)
    print(ist_obermenge fArr1 fArr2)
    print(ist_element (Fkt testAbb) fArr1)
    print(ist_leer fArr1)
    print(sind_gleich fArr1 fArr2)
    print(schneide fArr1 fArr2)
    print(ziehe_ab fArr1 fArr2)

    print("Aufgabe A.4")
    print(vereinige pArr1 pArr2)
    print(ist_teilmenge pArr1 pArr2)
    print(ist_obermenge pArr1 pArr2)
    print(ist_element (P (1,I)) pArr1)
    print(ist_leer pArr1)
    print(sind_gleich pArr1 pArr2)
    print(schneide pArr1 pArr2)
    print(ziehe_ab pArr1 pArr2)

    print(vereinige bArr1 bArr2)
    print(ist_teilmenge bArr1 bArr2)
    print(ist_obermenge bArr1 bArr2)
    print(ist_element (Blatt 2) bArr1)
    print(ist_leer bArr1)
    print(sind_gleich bArr1 bArr2)
    print(schneide bArr1 bArr2)
    print(ziehe_ab bArr1 bArr2)

    print("Aufgabe A.5")
    print(show (ET (Fkt testAbb)))
