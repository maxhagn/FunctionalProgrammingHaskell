module Angabe7 where


{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
   5. Ersetzen Sie die Trivialimplementierungen error "Nicht implementiert" durch
      sinnvolle Implementierungen, die die jeweilige Aufgabenstellung erfüllen.
   6. Kopieren Sie Ihre Implementierungen von Angabe 3 bzw. 6 an den
      entsprechenden Stellen ein. Beachten Sie, dass dafür drei Umbennennungen
      erforderlich sind, um Namenskonflikte zwischen Bezeichnungen von
      Angabe 3 und 6 zu vermeiden.
-}

import Data.List
import Data.Char
import Data.Maybe
import Data.Eq
import Prelude


type Nat0 = Int
type Nat1 = Int


-- Aufgabe A.1

-- Von Angabe 3 wiederholt:

-- Umbenennung 1: Zeile von Angabe 3 wird fuer Angabe 7 umbenannt zu Matrixzeile
type Matrixzeile = [Int]

-- Matrizen konzeptuell als Listen von Matrixzeilen dargestellt:
newtype Matrix = M [Matrixzeile]  

fehlerwert = M [] :: Matrix

data Matrixtyp = Mat (Nat1,Nat1) | KeineMatrix deriving (Eq,Show)

instance Show Matrix where
  show (M rows) = "(" ++ unwords (map show rows) ++ ")"

checkLineEquality :: [Matrixzeile] -> Bool
checkLineEquality [] = False
checkLineEquality (current:other) = all (\row -> length row == length current && length row /= 0) other

matrixtyp :: Matrix -> Matrixtyp
matrixtyp (M rows) | checkLineEquality rows == False = KeineMatrix
                     | otherwise = Mat (sum [ 1 | row <- head (tails rows), True ],
                       sum [ 1 | row <- head (tails rows!!0), True ])
 
instance Eq Matrix where
  (==) (M rows_1) (M rows_2) | matrixtyp (M rows_1) == KeineMatrix || matrixtyp (M rows_2) == KeineMatrix = error "Gleichheit undefiniert"
                             | otherwise = rows_1 == rows_2
  (/=) (M rows_1) (M rows_2) | matrixtyp (M rows_1) == KeineMatrix || matrixtyp (M rows_2) == KeineMatrix = error "Ungleichheit undefiniert"
                             | otherwise = rows_1 /= rows_2


checkMatrixRankEquality :: [Matrixzeile] -> [Matrixzeile] -> Bool
checkMatrixRankEquality rows_1 rows_2 | length rows_1 == length rows_2 && length (rows_1!!0) == length (rows_2!!0) = True | otherwise = False

matrixAddition :: Num a => [[a]] -> [[a]] -> [[a]]
matrixAddition = zipWith (zipWith (Prelude.+))

matrixSubtraction :: Num a => [[a]] -> [[a]] -> [[a]]
matrixSubtraction = zipWith (zipWith (Prelude.-))

matrixMultiplication :: Num a => [[a]] -> [[a]] -> [[a]]
matrixMultiplication rows_1 rows_2 = [ [ sum $ zipWith (Prelude.*) row_1 row_2 | row_2 <- (transpose rows_2) ] | row_1 <- rows_1 ]

matrixNegation :: [Matrixzeile] -> [Matrixzeile]
matrixNegation rows = [ [(value Prelude.* (-1)) | value <- row] | row <- rows]

matrixAbsolute :: [Matrixzeile] -> [Matrixzeile]
matrixAbsolute rows = [ [ if value < 0 then (value Prelude.* (-1)) else value | value <- row] | row <- rows]

matrixSignum :: [Matrixzeile] -> [Matrixzeile]
matrixSignum rows | checkMatrixRankEquality [ [ 1 | value <- row, value > 0] | row <- rows] rows = [[1]]
                  | checkMatrixRankEquality [ [ 1 | value <- row, value < 0] | row <- rows] rows = [[-1]]
                  | checkMatrixRankEquality [ [ 1 | value <- row, value == 0]| row <- rows] rows = [[0]]
                  | otherwise = error "Vorzeichenfunktion undefiniert"

instance Num Matrix where
 (M rows_1) + (M rows_2)   | (matrixtyp (M rows_1) == KeineMatrix || matrixtyp (M rows_2) == KeineMatrix) || not (checkMatrixRankEquality rows_1 rows_2) = fehlerwert
                          | otherwise = M (matrixAddition rows_1 rows_2)
 (M rows_1) - (M rows_2)   | (matrixtyp (M rows_1) == KeineMatrix || matrixtyp (M rows_2) == KeineMatrix) || not (checkMatrixRankEquality rows_1 rows_2) = fehlerwert
                          | otherwise = M (matrixSubtraction rows_1 rows_2)
 (M rows_1) * (M rows_2)   | (matrixtyp (M rows_1) == KeineMatrix || matrixtyp (M rows_2) == KeineMatrix) = fehlerwert
                          | otherwise = M (matrixMultiplication rows_1 rows_2)
 negate (M rows)           | (matrixtyp (M rows) == KeineMatrix) = fehlerwert
                          | otherwise = M (matrixNegation rows)
 abs (M rows)              | (matrixtyp (M rows) == KeineMatrix) = fehlerwert
                          | otherwise = M (matrixAbsolute rows)
 signum (M rows)           | (matrixtyp (M rows) == KeineMatrix) = fehlerwert
                          | otherwise = M (matrixSignum rows)
 fromInteger i = M [[(fromIntegral i)]]


-- Von Angabe 6 wiederholt:

type Zeilenzahl  = Nat1
type Spaltenzahl = Nat1
type Zeile       = Nat1
type Spalte      = Nat1
type Skalar      = Int

-- Umbenennung 2: Matrixtyp von Angabe 6 wird fuer Angabe 7 umbenannt zu Matrixtyp'
type Matrixtyp'  = (Zeilenzahl,Spaltenzahl)
type Matrixfkt   = Zeile -> Spalte -> Skalar  -- ausschliessl. total def. Abb.!

-- Matrizenwerte als Typ und funktionale Darstellung
data MatrixF = Mf { mtyp :: Matrixtyp', mf :: Matrixfkt }

-- Namesvereinbarung fuer den Fehlerwert des Typs MatrixF
fehler = Mf (0,0) (\_ _ -> 0) :: MatrixF

matrixToArray :: MatrixF -> [[Skalar]]
matrixToArray (Mf typ function) = [ [ function z s | s <- head (tails [1..(snd typ)]) ] | z <- head (tails [1..(fst typ)])]

instance Show MatrixF where
 show matrix = "(" ++ unwords (map show (matrixToArray matrix)) ++ ")"

-- Umbenennung 3: matrixtyp von Angabe 6 wird fuer Angabe 7 umbenannt zu matrixtyp'
matrixtyp' :: MatrixF -> Maybe Matrixtyp'
matrixtyp' matrix | length (matrixToArray matrix) > 0 && length ((matrixToArray matrix)!!0) > 0
                   = (Just ((length (matrixToArray matrix), length ((matrixToArray matrix)!!0))))
                  | otherwise = Nothing

instance Eq MatrixF where
  (Mf typ_1 function_1) == (Mf typ_2 function_2) | matrixtyp' (Mf typ_1 function_1) == Nothing
                             || matrixtyp' (Mf typ_2 function_2) == Nothing = error "Gleichheit undefiniert"
                           | otherwise = matrixToArray (Mf typ_1 function_1) == matrixToArray (Mf typ_2 function_2)
  (Mf typ_1 function_1) /= (Mf typ_2 function_2) | matrixtyp' (Mf typ_1 function_1) == Nothing
                             || matrixtyp' (Mf typ_2 function_2) == Nothing = error "Unleichheit undefiniert"
                           | otherwise = matrixToArray (Mf typ_1 function_1) /= matrixToArray (Mf typ_2 function_2)


instance Num MatrixF where
 (Mf t1 f1) + (Mf t2 f2) = error "Nicht implementiert!"
 (Mf t1 f1) - (Mf t2 f2) = error "Nicht implementiert!"
 (Mf t1 f1) * (Mf t2 f2) = error "Nicht implementiert!"
 negate (Mf t f)         = error "Nicht implementiert!"
 abs (Mf t f)            = error "Nicht implementiert!"
 signum (Mf t f)         = error "Nicht implementiert!"
 fromInteger n           = error "Nicht implementiert!"



-- Aufgabe A.2

class (Eq a,Num a,Show a) => MatrixTyp a where
 madd, msub, mmult :: a -> a -> a
 msmult  :: Int -> a -> a
 mtransp :: a -> a
 mdet    :: a -> Maybe Int
 mfehler :: a

 -- Protoimplementierungen
 madd  = (+)
 mmult = (*)
 msub  = (-)


instance MatrixTyp Matrix where
 msmult n (M zs) = error "Nicht implementiert!"
 mdet (M zs)     = error "Nicht implementiert!"

instance MatrixTyp MatrixF where
 msmult n (Mf t f) = error "Nicht implementiert!"
 mdet (Mf t f)     = error "Nicht implementiert!"


{- Knapp, aber gut nachvollziehbar gehen die Instanzbildungen fuer 
   MatrixTyp folgendermassen vor:
   ...
-}



-- Aufgabe A.3

konv1 :: Matrix -> MatrixF
konv1 (M zs) = error "Nicht implementiert!"

konv2 :: MatrixF -> Matrix
konv2 (Mf t f) = error "Nicht implementiert!"

{- Knapp, aber gut nachvollziehbar gehen die Konvertierungsfunktionen 
   konv1 und konv2 folgendermassen vor:
   ...
-}



-- Aufgabe A.6


type Konservenzahl = Nat0

type Bahnhof     = String
type Ausgangsbhf = Bahnhof
type Zielbhf     = Bahnhof
type Von         = Bahnhof
type Nach        = Bahnhof

data VHDS   = Viertel | Halb | Dreiviertel | Schlag 
               deriving (Eq,Ord,Enum,Show)
data Stunde = I | II | III | IV | V | VI | VII | VIII | IX | X
              | XI | XII | XIII | XIV | XV | XVI | XVII | XVIII
              | XIX | XX | XXI | XXII | XXIII | XXIV 
               deriving (Eq,Ord,Enum,Show)

data Abfahrtzeit = AZ { vds :: VHDS, std :: Stunde } deriving (Eq,Ord,Show)
data Reisedauer  = RD { vs :: Stunde, bt :: VHDS } deriving (Eq,Ord,Show)

type Fahrplan = [(Abfahrtzeit,Von,Nach,Reisedauer)]


konservenrechner :: Fahrplan -> Ausgangsbhf -> Zielbhf -> Maybe Konservenzahl
konservenrechner fp ab zb = error "Nicht implementiert!"

{- Knapp, aber gut nachvollziehbar geht die Funktion konservenrechner 
   folgendermassen vor:
   ...
-}

main = do
 print("")