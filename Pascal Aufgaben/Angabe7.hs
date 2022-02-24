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
 show (M z) = "(" ++ unwords (map show z) ++ ")"


sizeEqual :: [Matrixzeile] -> Bool
sizeEqual [] = False
sizeEqual (first:rest) = all (\elem -> length elem == length first && length elem /= 0) rest

matrixtyp :: Matrix -> Matrixtyp
matrixtyp (M z) | sizeEqual z == False = KeineMatrix
                | otherwise = Mat (sum [ 1 | p <- head (tails z)], sum [ 1 | p <- head (tails z!!0)])


instance Eq Matrix where
 (==) (M z1) (M z2) | matrixtyp (M z1) == KeineMatrix || matrixtyp (M z2) == KeineMatrix = error "Gleichheit undefiniert"
                    | otherwise = z1 == z2
 (/=) (M z1) (M z2) | matrixtyp (M z1) == KeineMatrix || matrixtyp (M z2) == KeineMatrix = error "Ungleichheit undefiniert"
                    | otherwise = z1 /= z2


zeilenUndSpaltenMatch :: [Matrixzeile] -> [Matrixzeile] -> Bool
zeilenUndSpaltenMatch z1 z2 | length z1 == length z2 && length (z1!!0) == length (z2!!0) = True
                            | otherwise = False

plus_mat :: Num a => [[a]] -> [[a]] -> [[a]]
plus_mat = zipWith (zipWith (Prelude.+))

minus_mat :: Num a => [[a]] -> [[a]] -> [[a]]
minus_mat = zipWith (zipWith (Prelude.-))

multi_mat :: Num a => [[a]] -> [[a]] -> [[a]]
multi_mat x y = multi_matT x $ transpose y

multi_matT :: Num a => [[a]] -> [[a]] -> [[a]]
multi_matT [] _ = []
multi_matT (a:as) b = calcRow a b : multi_matT as b

calcRow :: Num a => [a] -> [[a]] -> [a]
calcRow _ [] = []
calcRow a (b:bs) = calcCell a b : calcRow a bs

calcCell :: Num a => [a] -> [a] -> a
calcCell col row = foldl1 (Prelude.+) $ zipWith (Prelude.*) col row

neg_mat :: [Matrixzeile] -> [Matrixzeile]
neg_mat z = [ [(val Prelude.* (-1)) | val <- row] | row <- z]

abs_mat :: [Matrixzeile] -> [Matrixzeile]
abs_mat z = [ [ if val < 0 then (val Prelude.* (-1)) else val | val <- row] | row <- z]

signum_mat :: [Matrixzeile] -> [Matrixzeile]
signum_mat z | zeilenUndSpaltenMatch [ [ 1 | val <- row, val > 0] | row <- z] z = [[1]]
             | zeilenUndSpaltenMatch [ [ 1 | val <- row, val < 0] | row <- z] z = [[-1]]
             | zeilenUndSpaltenMatch [ [ 1 | val <- row, val == 0] | row <- z] z = [[0]]
             | otherwise = [[]]


instance Num Matrix where
 (M z1) + (M z2)   | (matrixtyp (M z1) == KeineMatrix || matrixtyp (M z2) == KeineMatrix) || not (zeilenUndSpaltenMatch z1 z2) = fehlerwert
                   | otherwise = M (plus_mat z1 z2)
 (M z1) - (M z2)   | (matrixtyp (M z1) == KeineMatrix || matrixtyp (M z2) == KeineMatrix) || not (zeilenUndSpaltenMatch z1 z2) = fehlerwert
                   | otherwise = M (minus_mat z1 z2)
 (M z1) * (M z2)   | (matrixtyp (M z1) == KeineMatrix || matrixtyp (M z2) == KeineMatrix) = fehlerwert
                   | otherwise = M (multi_mat z1 z2)
 negate (M z)      | (matrixtyp (M z) == KeineMatrix) = fehlerwert
                   | otherwise = M (neg_mat z)
 abs (M z)         | (matrixtyp (M z) == KeineMatrix) = fehlerwert
                   | otherwise = M (abs_mat z)
 signum (M z)      | (matrixtyp (M z) == KeineMatrix) = fehlerwert
                   | otherwise = M (signum_mat z)
 fromInteger i     = M [[(fromIntegral i)]]


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

calcMatrix :: MatrixF -> [[Skalar]]
calcMatrix (Mf t f) = [ [ f z s | s <- head (tails [1..(snd t)]) ] | z <- head (tails [1..(fst t)])]

instance Show MatrixF where
 show m = "(" ++ unwords (map show (calcMatrix m)) ++ ")"

-- Umbenennung 3: matrixtyp von Angabe 6 wird fuer Angabe 7 umbenannt zu matrixtyp'
matrixtyp' :: MatrixF -> Maybe Matrixtyp'
matrixtyp' m | length (calcMatrix m) > 0 && length ((calcMatrix m)!!0) > 0 = (Just ((length (calcMatrix m), length ((calcMatrix m)!!0))))
             | otherwise = Nothing

instance Eq MatrixF where
  (Mf t1 f1) == (Mf t2 f2) | matrixtyp' (Mf t1 f1) == Nothing || matrixtyp' (Mf t2 f2) == Nothing = error "Gleichheit undefiniert"
                           | otherwise = calcMatrix (Mf t1 f1) == calcMatrix (Mf t2 f2)
  (Mf t1 f1) /= (Mf t2 f2) | matrixtyp' (Mf t1 f1) == Nothing || matrixtyp' (Mf t2 f2) == Nothing = error "Unleichheit undefiniert"
                           | otherwise = calcMatrix (Mf t1 f1) /= calcMatrix (Mf t2 f2)

instance Num MatrixF where
  (Mf (z1, s1) f1) + (Mf (z2, s2) f2) | z1 == 0 || s1 == 0 || z1 /= z2 || s1 /= s2 = fehler
                                      | otherwise = Mf (z1,s1) (\z s -> f1 z s + f2 s z ) :: MatrixF
  (Mf (z1, s1) f1) - (Mf (z2, s2) f2) | z1 == 0 || s1 == 0 || z1 /= z2 || s1 /= s2 = fehler
                                      | otherwise = Mf (z1,s1) (\z s -> f1 z s - f2 s z ) :: MatrixF
  (Mf (z1, s1) f1) * (Mf (z2, s2) f2) | z1 == 0 || s1 == 0 || z2 == 0 || s2 == 0 || s1 /= z2 = fehler
                                      | otherwise = Mf (z1,s1) (\z s -> sum [f1 z x * f2 x s | x <- [1..s1]]) :: MatrixF
  negate (Mf (z1, s1) f)              | z1 == 0 || s1 == 0 = fehler
                                      | otherwise = Mf (z1,s1) (\z s -> (-1) * f z s) :: MatrixF
  abs (Mf (z1, s1) f)                 | z1 == 0 || s1 == 0 = fehler
                                      | otherwise = Mf (z1,s1) (\z s -> if f z s < 0 then (-1) * f z s
                                                                                   else f z s) :: MatrixF
  signum (Mf t f)                     = error "Nicht implementiert!"
  fromInteger n                       = Mf (1,1) (\z s -> (fromIntegral n)) :: MatrixF



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

scalarMult :: Int -> Matrix -> Matrix
scalarMult i (M zs) = (M [ [ i*val | val <- row] | row <- zs])

transpMatrix:: [[a]]->[[a]]
transpMatrix ([]:_) = []
transpMatrix x = (map head x) : transpMatrix (map tail x)

getRest :: Int -> [[a]] -> [[a]]
getRest n m = [ [ val | (j, val) <- zip [0..] row, j /= n] | (i, row) <- zip [0..] m, i /= 0]

cut :: [a] -> Int -> [a]
cut [ ] n = [ ]
cut xs n | n < 1 || n > (length xs) = xs
         | otherwise = (take (n-1) xs) ++ drop n xs

remove :: (Eq a) => [[a]] -> Int -> Int -> [[a]]
remove m i j | m == [ ] || i < 1 || i > length m || j < 1 || j > length (m!!0)  = error "remove:  (i,j) out of range"
             | otherwise = transpose ( cut (transpose ( cut m i ) ) j )

determinant :: [[Int]] -> Int
determinant [ ] = error "determinant: 0-by-0 matrix"
determinant [[n]] = n
determinant m = sum [ (-1)^ (j+1) * (head m)!!(j-1) * determinant (remove m 1 j) | j <- [1..(length (m!!0)) ] ]

zeilenUndSpaltenMatch' :: [[a]] -> Bool
zeilenUndSpaltenMatch' z1   | length z1 == length (z1!!0) = True
                            | otherwise = False

instance MatrixTyp Matrix where
 msmult n (M zs) | (matrixtyp (M zs)) == KeineMatrix = fehlerwert
                 | otherwise = scalarMult n (M zs)
 mtransp (M zs)  | (matrixtyp (M zs)) == KeineMatrix = fehlerwert
                 | otherwise = (M (transpMatrix zs))
 mdet (M zs)     | (matrixtyp (M zs)) == KeineMatrix || not (zeilenUndSpaltenMatch' zs) = Nothing
                 | otherwise = Just (determinant zs)
 mfehler         = fehlerwert

instance MatrixTyp MatrixF where
  msmult n (Mf (z1, s1) f1) | matrixtyp' (Mf (z1, s1) f1) == Nothing = fehler
                            | otherwise = Mf (z1,s1) (\z s -> (f1 z s) * n) :: MatrixF
  mtransp (Mf (z1, s1) f1)  | matrixtyp' (Mf (z1, s1) f1) == Nothing = fehler
                            | otherwise = Mf (z1,s1) (\z s -> (f1 s z)) :: MatrixF
  mdet (Mf (z1, s1) f1)     | matrixtyp' (Mf (z1, s1) f1) == Nothing || z1 /= s1 = Nothing
                            | otherwise = Just (determinant (calcMatrix (Mf (z1, s1) f1)))
  mfehler                   = fehler


{- Knapp, aber gut nachvollziehbar gehen die Instanzbildungen fuer
   MatrixTyp folgendermassen vor:
   Hilfsfunktionen für scalar Multiplikation, transpose und determinantenbestimmung für Matrix geschrieben
   Dabei meistens einfache iteration.
   Für MatrixF erstellung neuer Funktionen (einfach multiplizieren, Parameter umdrehen, determinante wie bei Matrix)
-}



-- Aufgabe A.3

konv1 :: Matrix -> MatrixF
konv1 (M zs) | (matrixtyp (M zs)) == KeineMatrix = fehler
             | otherwise = Mf ((length zs),(length (zs!!0))) (\z s -> ((zs!!(z-1))!!(s-1))) :: MatrixF

konv2 :: MatrixF -> Matrix
konv2 (Mf (z1, s1) f) | matrixtyp' (Mf (z1, s1) f) == Nothing = fehlerwert
                      | otherwise = (M (calcMatrix (Mf (z1, s1) f)))

{- Knapp, aber gut nachvollziehbar gehen die Konvertierungsfunktionen
   konv1 und konv2 folgendermassen vor:
   Matrix -> MatrixF : Wertzuweisung und länge über das Matrixarray bestimmen
   MatrixF -> Matrix : Einfach calcMatrix aufrufen
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

data Umstieg = U { von :: Bahnhof, nach :: Bahnhof } deriving (Eq,Ord,Show)

data Weg = W { weg :: [Umstieg], gTage :: Nat0 } deriving (Eq,Ord,Show)



konservenrechner :: Fahrplan -> Ausgangsbhf -> Zielbhf -> Maybe Konservenzahl
konservenrechner fp ab zb = error "Nicht implementiert!"

{- Knapp, aber gut nachvollziehbar geht die Funktion konservenrechner
   folgendermassen vor:
   ...
-}


main = do
  print(scalarMult 5 (M [[2,3], [4,5]]))
  print(transpMatrix [[2,3], [4,5]])
  print(konv1 (M [[1,5,2], [2,2,2], [4,3,3]]))
