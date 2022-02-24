> module Angabe3 where

1. Vervollstaendigen Sie gemaess Angabentext!
2. Vervollst�ndigen Sie auch die vorgegebenen Kommentaranf�nge!
3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!

> import Data.List
> import Data.Char
> import Data.Maybe
> import Data.Eq
> import Prelude


> type Nat1      = Int
> type Zeile     = [Int]

Matrizen konzeptuell als Listen von Zeilen dargestellt:

> newtype Matrix = M [Zeile]

> fehlerwert = M [] :: Matrix

> data Matrixtyp = Mat (Nat1,Nat1) | KeineMatrix deriving (Eq,Show)


Aufgabe A.1

> instance Show Matrix where
>  show (M z) = "(" ++ unwords (map show z) ++ ")"

Knapp, aber gut nachvollziebar geht die Instanzdeklaration fuer Show folgendermassen vor:
Durch die unwords/map kombination wird das äußere Array so hingehend geändert, dass beim show nur die einzelnen
Unterarrays als solche angezeigt werden. Die inneren Arrays werden mit dem innersten show einfach so angezeigt.


Aufgabe A.2

> sizeEqual :: [Zeile] -> Bool
> sizeEqual [] = False
> sizeEqual (first:rest) = all (\elem -> length elem == length first && length elem /= 0) rest

> matrixtyp :: Matrix -> Matrixtyp
> matrixtyp (M z) | sizeEqual z == False = KeineMatrix
>                 | otherwise = Mat (sum [ 1 | p <- head (tails z)], sum [ 1 | p <- head (tails z!!0)])

Knapp, aber gut nachvollziehbar geht matrixtyp folgendermassen vor:
Hier wird die Funktion sizeEqual verwendet um zu prüfen ob alle Unterarrays vom gleichen aufbau sind.
Ist das der Fall wird ein data Matrixtyp erstellt wo vom äußersten und innersten Array die länge bestimmt wird.
(Vielleich bisschen Umständlich :))

Aufgabe A.3

> instance Eq Matrix where
>  (==) (M z1) (M z2) | matrixtyp (M z1) == KeineMatrix || matrixtyp (M z2) == KeineMatrix = error "Gleichheit undefiniert"
>                     | otherwise = z1 == z2
>  (/=) (M z1) (M z2) | matrixtyp (M z1) == KeineMatrix || matrixtyp (M z2) == KeineMatrix = error "Ungleichheit undefiniert"
>                     | otherwise = z1 /= z2

Knapp, aber gut nachvollziebar geht die Instanzdeklaration fuer Eq folgendermassen vor:
Beim Eq wird für Matrizen zuerst geprüft ob es eine gültige Matrix ist, andernfalls wird geprüft ob die
Listen aus denen die Matrix besteht gleich sind. Ditto für ungleichheit.

Aufgabe A.4

> zeilenUndSpaltenMatch :: [Zeile] -> [Zeile] -> Bool
> zeilenUndSpaltenMatch z1 z2 | length z1 == length z2 && length (z1!!0) == length (z2!!0) = True
>                             | otherwise = False

> plus_mat :: Num a => [[a]] -> [[a]] -> [[a]]
> plus_mat = zipWith (zipWith (Prelude.+))

> minus_mat :: Num a => [[a]] -> [[a]] -> [[a]]
> minus_mat = zipWith (zipWith (Prelude.-))

> multi_mat :: Num a => [[a]] -> [[a]] -> [[a]]
> multi_mat x y = multi_matT x $ transpose y

> multi_matT :: Num a => [[a]] -> [[a]] -> [[a]]
> multi_matT [] _ = []
> multi_matT (a:as) b = calcRow a b : multi_matT as b

> calcRow :: Num a => [a] -> [[a]] -> [a]
> calcRow _ [] = []
> calcRow a (b:bs) = calcCell a b : calcRow a bs

> calcCell :: Num a => [a] -> [a] -> a
> calcCell col row = foldl1 (Prelude.+) $ zipWith (Prelude.*) col row

> neg_mat :: [Zeile] -> [Zeile]
> neg_mat z = [ [(val Prelude.* (-1)) | val <- row] | row <- z]

> abs_mat :: [Zeile] -> [Zeile]
> abs_mat z = [ [ if val < 0 then (val Prelude.* (-1)) else val | val <- row] | row <- z]

> signum_mat :: [Zeile] -> [Zeile]
> signum_mat z | zeilenUndSpaltenMatch [ [ 1 | val <- row, val > 0] | row <- z] z = [[1]]
>              | zeilenUndSpaltenMatch [ [ 1 | val <- row, val < 0] | row <- z] z = [[-1]]
>              | zeilenUndSpaltenMatch [ [ 1 | val <- row, val == 0] | row <- z] z = [[0]]
>              | otherwise = [[]]


> instance Num Matrix where
>  (M z1) + (M z2)   | (matrixtyp (M z1) == KeineMatrix || matrixtyp (M z2) == KeineMatrix) || not (zeilenUndSpaltenMatch z1 z2) = fehlerwert
>                    | otherwise = M (plus_mat z1 z2)
>  (M z1) - (M z2)   | (matrixtyp (M z1) == KeineMatrix || matrixtyp (M z2) == KeineMatrix) || not (zeilenUndSpaltenMatch z1 z2) = fehlerwert
>                    | otherwise = M (minus_mat z1 z2)
>  (M z1) * (M z2)   | (matrixtyp (M z1) == KeineMatrix || matrixtyp (M z2) == KeineMatrix) = fehlerwert
>                    | otherwise = M (multi_mat z1 z2)
>  negate (M z)      | (matrixtyp (M z) == KeineMatrix) = fehlerwert
>                    | otherwise = M (neg_mat z)
>  abs (M z)         | (matrixtyp (M z) == KeineMatrix) = fehlerwert
>                    | otherwise = M (abs_mat z)
>  signum (M z)      | (matrixtyp (M z) == KeineMatrix) = fehlerwert
>                    | otherwise = M (signum_mat z)
>  fromInteger i     = M [[(fromIntegral i)]]



Knapp, aber gut nachvollziebar geht die Instanzdeklaration fuer Num folgendermassen vor:
Für die + und - Funktionen der Instanzklassendeklaration wurden Hilfsfunktionen geschrieben sie sehr ähnlich
mit Zipwith die einzelnen Zellen der Matrix addieren und subtrahieren.
Multiplikation ist etwas komplexer. Hier wird transposiniert und dann Reihenweise die Zellen multipliziert.
Bei neg und abs werden neue zweidimensionale Arrays erstellt, bei denen alle bzw. die benötigten Werte mit -1
multipliziert werden.
signum erstellt arrays bei denen einmal alle > 0, alle < 0 und alle == 0 zu einem neuen Array gekapselt werden.
Dann kann mit der zeilenUndSpaltenMatch funktion gecheckt werden ob tatsächlich alle werte im neuen Array sind
(also für alle positiven, negativen und 0 Werte)
fromInteger erstellt einfach eine Matrix mit dem vorgegebenen Int.




> main = do
>    print("Aufgabe A.1")
>    print(show (M [[1,2],[3,4]])) -- "([1,2] [3,4])"
>    print(show (M [[1,2],[3,4],[5,6]])) -- "([1,2] [3,4] [5,6])"
>    print(show (M [[1,2,3],[4,5],[6]])) -- "([1,2,3] [4,5] [6])"
>    print(show (M [[1,2,3],[],[6]])) -- "([1,2,3] [] [6])"
>    print(show (M [[],[],[]])) -- "([] [] [])"
>    print(show (M [])) -- "()"

>    print("Aufgabe A.2")
>    print(matrixtyp (M [[1,2],[3,4],[5,6]]))
>    print(matrixtyp (M [[1,2,3],[],[6]]))
>    print(matrixtyp (M [[],[],[]]))
>    print(matrixtyp (M [[1,1,1]]))

>    print("Aufgabe A.3")
>    print(M [[1,2],[3,4]] == M [[1,2],[3,4]])
>    print(M [[1,1],[3,4]] == M [[1,2],[3,4]])

>    print("Aufgabe A.4")
>    print(M [[1,2],[3,4]] Prelude.+ M [[1,2],[3,4]])
>    print(M [[1,2],[3,4]] Prelude.+ M [[1,2],[3,4,5]])
>    print(M [[1,2],[3,4]] Prelude.- M [[1,2],[3,4]])
>    print(M [[1,2],[3,4]] Prelude.- M [[2,2],[3,4]])
>    print(M [[1,2],[3,4]] Prelude.- M [[1,2],[3,4,5]])
>    print(M [[1,2],[3,4]] Prelude.* M [[1,2],[3,4]])
>    print(M [[1,2],[3,4]] Prelude.* M [[2,2],[3,4]])
>    print(M [[1,2],[3,4]] Prelude.* M [[1,2],[3,4,5]])
>    print(Prelude.negate (M [[1,2],[3,4]]))
>    print(Prelude.abs (M [[-2,4], [3,-6]]))
>    print(Prelude.signum (M [[-2,4], [3,-6]]))
>    print(Prelude.signum (M [[2,4], [3,6]]))
>    print(Prelude.signum (M [[0,0], [0,0]]))
>    print(Prelude.fromInteger 69)

>    print("---------------------------------")
>    print(M [[1 .. 3], [4 .. 6], [7 .. 9]] Prelude.+ (M [[9, 8, 7], [6, 5, 4], [3, 2, 1]]))
>    print((M [[1 .. 3]] Prelude.+ M [[1], [2], [3]]))
>    print(M [[1 .. 3], [4 .. 6], [7 .. 9]] Prelude.- M [[1 .. 3], [4 .. 6], [7 .. 9]])
>    print((M [[1 .. 3]] Prelude.- M [[1], [2], [3]]))
>    print(M [[1 .. 3], [4 .. 6], [7 .. 9]] Prelude.* M [[1, 0, 0], [0, 1, 0], [0, 0, 1]])
>    print(M [[1 .. 3]] Prelude.* M [[1], [2], [3]])
>    print((M [[1 .. 3]] Prelude.* M [[1, 3], [2], [3]]))
>    print(Prelude.negate (M [[1 .. 3], [-4, -5, -6]]))
>    print(Prelude.abs (M [[1 .. 3], [-4, -5, -6]]))
>    print(Prelude.signum (M [[1 .. 3], [4 .. 6]]))
>    print(Prelude.signum (M [[-1], [-2]]))
>    print(Prelude.fromInteger 4)
