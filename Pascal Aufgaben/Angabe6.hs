module Angabe6 where

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
   5. Ersetzen Sie die Trivialimplementierungen error "Nicht implementiert" durch
      sinnvolle Implementierungen, die die jeweilige Aufgabenstellung erfüllen.
-}

import Data.List
import Data.Char

type Nat0        = Int
type Nat1        = Int
type Zeilenzahl  = Nat1
type Spaltenzahl = Nat1
type Zeile       = Nat1
type Spalte      = Nat1
type Skalar      = Int
type Matrixtyp   = (Zeilenzahl,Spaltenzahl)
type Matrixfkt   = Zeile -> Spalte -> Skalar  -- ausschliessl. total def. Abb.!

-- Matrizenwerte als Typ und funktionale Darstellung
data MatrixF = Mf { mtyp :: Matrixtyp, mf :: Matrixfkt }

-- Namesvereinbarung fuer den Fehlerwert
fehler = Mf (0,0) (\_ _ -> 0) :: MatrixF



-- Aufgabe A.1

calcMatrix :: MatrixF -> [[Skalar]]
calcMatrix (Mf t f) = [ [ f z s | s <- head (tails [1..(snd t)]) ] | z <- head (tails [1..(fst t)])]

instance Show MatrixF where
 show m = "(" ++ unwords (map show (calcMatrix m)) ++ ")"


{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer Show folgendermassen vor:
   ...
-}



-- Aufgabe A.2



matrixtyp :: MatrixF -> Maybe Matrixtyp
matrixtyp m | length (calcMatrix m) > 0 && length ((calcMatrix m)!!0) > 0 = (Just ((length (calcMatrix m), length ((calcMatrix m)!!0))))
            | otherwise = Nothing



{- Knapp, aber gut nachvollziehbar geht natrixtyp folgendermassen vor:
   ...
-}



-- Aufgabe A.4

instance Eq MatrixF where
 (Mf t1 f1) == (Mf t2 f2) | matrixtyp (Mf t1 f1) == Nothing || matrixtyp (Mf t2 f2) == Nothing = error "Gleichheit undefiniert"
                          | otherwise = calcMatrix (Mf t1 f1) == calcMatrix (Mf t2 f2)
 (Mf t1 f1) /= (Mf t2 f2) | matrixtyp (Mf t1 f1) == Nothing || matrixtyp (Mf t2 f2) == Nothing = error "Unleichheit undefiniert"
                          | otherwise = calcMatrix (Mf t1 f1) /= calcMatrix (Mf t2 f2)

{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer Eq folgendermassen vor:
   ...
-}



-- Aufgabe A.5


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


{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer Num folgendermassen vor:
   ...
-}





-- m1 bis m4 sind gleichwertig und entsprechen M [[1,2],[3,4]] von Angabe 3
m1 = Mf (2,2) (\z s -> if z == 1 then s else z+s) :: MatrixF
m2 = Mf (2,2) (\z s -> s + ((z-1)*(snd (2,2)))) :: MatrixF
m3 = Mf (2,2) (\z s -> s + ((z-1)*(snd (mtyp m2)))) :: MatrixF
m4 = Mf (2,2) (\z s -> if z == 1 then (succ (fib (s-1)))
                                 else ((+) z (binom z (s-1)))) :: MatrixF

-- m5, m6 sind gleichwertig und entsprechen M [[1,2],[3,4],[5,6]] von Angabe 3
m5 = Mf (3,2) (\z s -> if z == 1 then s
                                 else if z == 2 then z+s
                                                else succ (z+s)) :: MatrixF
m6 = Mf (3,2) (\z s -> s + ((z-1)*(snd (mtyp m5)))) :: MatrixF

-- m7, m8, alle anderen Matrixwerte mit Typ (0,0) entsprechen M [] v. Angabe 3
m7 = Mf (0,0) (\_ _ -> 0) :: MatrixF
m8 = Mf (0,0) (\z s -> z+s) :: MatrixF

-- Fur m4 verwendete Hilfsfunktionen
fib :: Nat0 -> Nat0
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)
binom :: Nat0 -> Nat0 -> Nat1
binom n k | n==0 || n==k = 1
          | True = binom (n-1) (k-1) + binom (n-1) k



main = do
 -- Gewunschte Matrixdarstellungen als Zeichenreihen (ident zu Angabe 3)
 print("Aufgabe A.1")
 print(show m1) -- "([1,2] [3,4])"
 print(show m2) -- "([1,2] [3,4])"
 print(show m3) -- "([1,2] [3,4])"
 print(show m4) -- "([1,2] [3,4])"
 print(show m5) -- "([1,2] [3,4] [5,6])"
 print(show m6) -- "([1,2] [3,4] [5,6])"
 print(show m7) -- "()"
 print(show m8) -- "()"

 print("Aufgabe A.2")
 print(matrixtyp m1)

 print("Aufgabe A.4")
 print(m1 == m3)
 print(m1 /= m7)
