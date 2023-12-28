> module Angabe3 where

1. Vervollstaendigen Sie gemaess Angabentext!
2. Vervollst�ndigen Sie auch die vorgegebenen Kommentaranf�nge!
3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
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
>   show (M rows) = "(" ++ unwords (map show rows) ++ ")"


Knapp, aber gut nachvollziebar geht die Instanzdeklaration fuer Show folgendermassen vor:
Die Zeilen werden mit Map Show dargestellt und anschließend mit unwords die Klammerungen entfernt
und mit Leerzeilen aneinander gereit. Schlussendlich wird die ganze Zeilenkette mit Klammern versehen.


Aufgabe A.2

> checkLineEquality :: [Zeile] -> Bool
> checkLineEquality [] = False
> checkLineEquality (current:other) = all (\row -> length row == length current && length row /= 0) other

> matrixtyp :: Matrix -> Matrixtyp
> matrixtyp (M rows) | checkLineEquality rows == False = KeineMatrix
>                      | otherwise = Mat (sum [ 1 | row <- head (tails rows), True ],
>                        sum [ 1 | row <- head (tails rows!!0), True ])

Knapp, aber gut nachvollziehbar geht matrixtyp folgendermassen vor:
Die Funktion nimmt eine Matrix entgegen und gibt einen matrixtyp zurück.
Die Funktion checkLineEquality überprüft, ob die Anzahl der Zeilen in jeder Reihe gleich ist
und somit eine gültige Matrix vorliegt. Sind nicht alle Reihen gleich, oder ist eine Zeile leer
wird KeineMatrix zurückgegben. Ist die Matrix gültig werden die Zeilen und Spalten gezählt und in einem
Tupel ausgegeben.
 


Aufgabe A.3

> instance Eq Matrix where
>   (==) (M rows_1) (M rows_2) | matrixtyp (M rows_1) == KeineMatrix || matrixtyp (M rows_2) == KeineMatrix = error "Gleichheit undefiniert"
>                              | otherwise = rows_1 == rows_2
>   (/=) (M rows_1) (M rows_2) | matrixtyp (M rows_1) == KeineMatrix || matrixtyp (M rows_2) == KeineMatrix = error "Ungleichheit undefiniert"
>                              | otherwise = rows_1 /= rows_2

Knapp, aber gut nachvollziebar geht die Instanzdeklaration fuer Eq folgendermassen vor:
Sowohl bei Gleichheit als auch bei Ungleichheit wird vorerst überprüft ob beide Matrizen gültig sind.
Ist eine der angegebenen Matrizen nicht gültig wird ein error mit Ungleichheit/Gleichheit undefiniert ausgegeben.
Sind die Matrizen gültig wird überprüft ob die Arrays der Zeilen gleich sind.



Aufgabe A.4

> checkMatrixRankEquality :: [Zeile] -> [Zeile] -> Bool
> checkMatrixRankEquality rows_1 rows_2 | length rows_1 == length rows_2 && length (rows_1!!0) == length (rows_2!!0) = True | otherwise = False

> matrixAddition :: Num a => [[a]] -> [[a]] -> [[a]]
> matrixAddition = zipWith (zipWith (Prelude.+))

> matrixSubtraction :: Num a => [[a]] -> [[a]] -> [[a]]
> matrixSubtraction = zipWith (zipWith (Prelude.-))

> matrixMultiplication :: Num a => [[a]] -> [[a]] -> [[a]]
> matrixMultiplication rows_1 rows_2 = [ [ sum $ zipWith (Prelude.*) row_1 row_2 | row_2 <- (transpose rows_2) ] | row_1 <- rows_1 ]

> matrixNegation :: [Zeile] -> [Zeile]
> matrixNegation rows = [ [(value Prelude.* (-1)) | value <- row] | row <- rows]

> matrixAbsolute :: [Zeile] -> [Zeile]
> matrixAbsolute rows = [ [ if value < 0 then (value Prelude.* (-1)) else value | value <- row] | row <- rows]

> matrixSignum :: [Zeile] -> [Zeile]
> matrixSignum rows | checkMatrixRankEquality [ [ 1 | value <- row, value > 0] | row <- rows] rows = [[1]]
>                   | checkMatrixRankEquality [ [ 1 | value <- row, value < 0] | row <- rows] rows = [[-1]]
>                   | checkMatrixRankEquality [ [ 1 | value <- row, value == 0]| row <- rows] rows = [[0]]
>                   | otherwise = error "Vorzeichenfunktion undefiniert"

> instance Num Matrix where
>  (M rows_1) + (M rows_2)   | (matrixtyp (M rows_1) == KeineMatrix || matrixtyp (M rows_2) == KeineMatrix) || not (checkMatrixRankEquality rows_1 rows_2) = fehlerwert
>                           | otherwise = M (matrixAddition rows_1 rows_2)
>  (M rows_1) - (M rows_2)   | (matrixtyp (M rows_1) == KeineMatrix || matrixtyp (M rows_2) == KeineMatrix) || not (checkMatrixRankEquality rows_1 rows_2) = fehlerwert
>                           | otherwise = M (matrixSubtraction rows_1 rows_2)
>  (M rows_1) * (M rows_2)   | (matrixtyp (M rows_1) == KeineMatrix || matrixtyp (M rows_2) == KeineMatrix) = fehlerwert
>                           | otherwise = M (matrixMultiplication rows_1 rows_2)
>  negate (M rows)           | (matrixtyp (M rows) == KeineMatrix) = fehlerwert
>                           | otherwise = M (matrixNegation rows)
>  abs (M rows)              | (matrixtyp (M rows) == KeineMatrix) = fehlerwert
>                           | otherwise = M (matrixAbsolute rows)
>  signum (M rows)           | (matrixtyp (M rows) == KeineMatrix) = fehlerwert
>                           | otherwise = M (matrixSignum rows)
>  fromInteger i = M [[(fromIntegral i)]]

Knapp, aber gut nachvollziebar geht die Instanzdeklaration fuer Num folgendermassen vor: 
Für alle Matrizen wird zuerst geprüft, ob es sich um gültige Matrizen handelt, indem matrixtyp aufgerufen wird.
Bei der Addition und Subtraktion wird zusätzlich überprüft, ob die Ränge der beiden Matrizen gleich sind.
Ist eine Matrix ungültig, oder die Ränge sind nicht ident wird der fehlerwert zurückgegeben.
Bei der Addtition werden die einzelnen Werte in einer Zeile mit denen der anderen Matrix addiert.
Bei der Subtraktion werden die einzelnen Werte in einer Zeile mit denen der anderen Matrix subtrahiert.
Bei der Multiplikation wird immer eine Zeile transponiert und die Werte mit dazugehörigen Zeile der anderen
Matrix multipliziert und summiert.
Bei Negate wird jeder Wert in der Matrix mit -1 multipliziert, um die Vorzeichen umzudrehen.
Bei Absolute wird jeder Wert in der Matrix der kleiner als 0 ist mit -1 multipliziert, damit jeder Wert absolut wird.
Signum überprüft, ob alle Werte der Matrix ein betimmtes Vorzeichen besitzen oder 0 sind und retouniert dementsprechend
1,0 oder -1. Wenn nicht alle Werte das selbe Vorzeichen besitzen wird error "Vorzeichenfunktion undefiniert" retouniert.
FromInteger retouniert eine Matrix mit dem angegebenen Wert als einziges Element.

> main = do
>  print("")