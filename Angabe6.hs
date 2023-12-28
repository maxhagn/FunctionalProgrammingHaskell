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
import Data.Maybe
import Data.Eq
import Prelude

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

matrixToArray :: MatrixF -> [[Skalar]]
matrixToArray (Mf typ function) = [ [ function z s | s <- head (tails [1..(snd typ)]) ] | z <- head (tails [1..(fst typ)])]

instance Show MatrixF where
 show matrix = "(" ++ unwords (map show (matrixToArray matrix)) ++ ")"


{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer Show folgendermassen vor:
   Die Instanzbildung für Show einer MatrixF zeigt die Matrix in Klammern dargestellt an.
   maxtrixToArray nimmt eine MatrixF entgegen und verwandelt diese in ein Array von Abbildungen.
   Die Abbildungen werden jeweils in eckigen Klammern umschlossen und als Array zurückgegeben.
-}



-- Aufgabe A.2

matrixtyp :: MatrixF -> Maybe Matrixtyp
matrixtyp matrix | length (matrixToArray matrix) > 0 && length ((matrixToArray matrix)!!0) > 0
                   = (Just ((length (matrixToArray matrix), length ((matrixToArray matrix)!!0))))
                 | otherwise = Nothing


{- Knapp, aber gut nachvollziehbar geht natrixtyp folgendermassen vor:
   Die Funktion matrixtyp nimmt eine MatrixF engegen und gibt vielleicht einen Matrixtyp zurück.
   Vorerst wird geprüft, ob die Matrix größer als 0 ist und ob m und n größer als 0 sind, ist dies
   der Fall wird der Wert Just((m,n)) zurückgegeben. Andererfalls wird der Wert Nothing retourniert.
-}



-- Aufgabe A.4

instance Eq MatrixF where
  (Mf typ_1 function_1) == (Mf typ_2 function_2) | matrixtyp (Mf typ_1 function_1) == Nothing
                             || matrixtyp (Mf typ_2 function_2) == Nothing = error "Gleichheit undefiniert"
                           | otherwise = matrixToArray (Mf typ_1 function_1) == matrixToArray (Mf typ_2 function_2)
  (Mf typ_1 function_1) /= (Mf typ_2 function_2) | matrixtyp (Mf typ_1 function_1) == Nothing
                             || matrixtyp (Mf typ_2 function_2) == Nothing = error "Unleichheit undefiniert"
                           | otherwise = matrixToArray (Mf typ_1 function_1) /= matrixToArray (Mf typ_2 function_2)

{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer Eq folgendermassen vor:
   Die Instanzbildung für Eq überprüft, pb zwei Matrizen gleich oder ungleich sind.
   Wenn eine oder beide Matrizen keiner gültigen Definition entsprechen wird im Falle
   von == "Gleichheit undefiniert" zurückgegeben. Analog dazu wird bei /= "Ungleichheit ungefiniert" retourniert.
   Sind die angegebenen Matrizen gültig wird, wird überprüft, ob alle Abbildungen gleich sind.
-}



-- Aufgabe A.5

instance Num MatrixF where
 (Mf t1 f1) + (Mf t2 f2) = error "Nicht implementiert!"
 (Mf t1 f1) - (Mf t2 f2) = error "Nicht implementiert!"
 (Mf t1 f1) * (Mf t2 f2) = error "Nicht implementiert!"
 negate (Mf t f)         = error "Nicht implementiert!"
 abs (Mf t f)            = error "Nicht implementiert!"
 signum (Mf t f)         = error "Nicht implementiert!"
 fromInteger n           = error "Nicht implementiert!"


{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer Num folgendermassen vor:
   ...
-}

main = do
 print("")