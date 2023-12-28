module Angabe5 where

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
-}

import Data.List
import Data.Char
import Data.Maybe
import Data.Eq
import Prelude


type Nat0 = Int
type Nat1 = Int


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
data PH_ElemTyp a b c d e = A a | B b | C c | D d | E e deriving (Eq,Show)
data PH_ElemTyp' q r s    = Q q | R r | S s deriving (Eq,Show)

-- Aufgabe A.1

romanToNat0 :: Zahlraum_0_10 -> Nat0
romanToNat0 number
 | number == N     = 0
 | number == I     = 1
 | number == II    = 2
 | number == III   = 3
 | number == IV    = 4
 | number == V     = 5
 | number == VI    = 6
 | number == VII   = 7
 | number == VIII  = 8
 | number == IX    = 9
 | number == X     = 10
 | otherwise = 99

nat0ToRoman :: Nat0 -> Zahlraum_0_10
nat0ToRoman number
  | number == 0  = N
  | number == 1  = I
  | number == 2  = II
  | number == 3  = III
  | number == 4  = IV
  | number == 5  = V
  | number == 6  = VI
  | number == 7  = VII
  | number == 8  = VIII
  | number == 9  = IX
  | number == 10 = X
  | otherwise = F

instance Num Zahlraum_0_10 where
  a + b         = nat0ToRoman ( romanToNat0 a + romanToNat0 b )
  a - b         = nat0ToRoman ( romanToNat0 a - romanToNat0 b )
  a * b         = nat0ToRoman ( romanToNat0 a * romanToNat0 b )
  abs a         = a
  signum a      = I
  negate a      = a
  fromInteger i = nat0ToRoman(fromIntegral i)

{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer Num folgendermassen vor:
   Die Instanzbildung nimmt eine Operation auf einen Zahlenraum entgegen und führt diese Operation aus. Die römischen Zahlen werden
   dafür in natürliche Zahlen konvertiert und anschließend die Berechnung ausgeführt. Das Ergebnis wird anschließend wieder in eine
   römische Zahl konvertiert, wobei F zurück gegeben wird, wenn das Ergebnis nicht im Zahlenraum liegt.
-}

-- Aufgabe A.2

function :: (Zahlraum_0_10 -> Zahlraum_0_10) -> Bild
function func = Bi [ (number, func number) | number <- head (tails [N, I, II, III, IV, V, VI, VII, VIII, IX, X, F]) ]

data Bild = Bi [(Zahlraum_0_10,Zahlraum_0_10)] deriving (Eq,Ord)

instance Show Bild where
  show (Bi model) = show model

instance Eq Funktion where
  Fkt func_1 == Fkt func_2 = function func_1 == function func_2
  Fkt func_1 /= Fkt func_2 = function func_1 /= function func_2

instance Show Funktion where
  show (Fkt func) = "{" ++ tail (init (show (function func))) ++ "}"

{- Knapp, aber gut nachvollziehbar gehen die beiden Instanzbildungen fuer Eq und Show folgendermassen vor:
   Die Instanzbildung für show führt die angegebene Funktion function mit allen Werten im Zahlenraum_0_10 aus und gibt für jeden Wert eine Abbildung zurück.
   Die Instanzbildung für Eq überprüft ob alle Abbildungen der beiden angegebenen Funktionen gleich sind.
-}

-- Aufgabe A.3

isSet :: (Ord a) => [a] -> Bool
isSet a = length (nub a) == length a

countNumber :: Eq a => a -> [a] -> Nat0
countNumber a = length . filter (a==)

instance Ord Funktion where
  (Fkt func_1) `compare` (Fkt func_2) = function func_1 `compare` function func_2

instance Menge_von Int where
   leer                      = []
   vereinige set_1 set_2     | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = set_1 ++ set_2
   schneide set_1 set_2      | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = let ns = [ x | x <- set_1, elem x set_2] in [ y | y <- set_2, elem y ns]
   ziehe_ab set_1 set_2      | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = set_1 \\ set_2
   ist_teilmenge set_1 set_2 | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = sum [ 1 | i <- head (tails set_1), ist_element i set_2] == length set_1
   ist_obermenge set_1 set_2 | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = ist_teilmenge set_2 set_1
   ist_element number set_1  | not (isSet set_1)                      = error "Fehler" | otherwise = countNumber number set_1 >= 1
   ist_leer set_1            | not (isSet set_1)                      = error "Fehler" | otherwise = set_1 == leer
   sind_gleich set_1 set_2   | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = ist_teilmenge set_1 set_2 && ist_teilmenge set_2 set_1
   anzahl number set_1       = countNumber number set_1

instance Menge_von Zahlraum_0_10 where
   leer                      = []
   vereinige set_1 set_2     | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = set_1 ++ set_2
   schneide set_1 set_2      | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = let ns = [ x | x <- set_1, elem x set_2] in [ y | y <- set_2, elem y ns]
   ziehe_ab set_1 set_2      | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = set_1 \\ set_2
   ist_teilmenge set_1 set_2 | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = sum [ 1 | i <- head (tails set_1), ist_element i set_2] == length set_1
   ist_obermenge set_1 set_2 | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = ist_teilmenge set_2 set_1
   ist_element number set_1  | not (isSet set_1)                      = error "Fehler" | otherwise = countNumber number set_1 >= 1
   ist_leer set_1            | not (isSet set_1)                      = error "Fehler" | otherwise = set_1 == leer
   sind_gleich set_1 set_2   | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = ist_teilmenge set_1 set_2 && ist_teilmenge set_2 set_1
   anzahl number set_1       = countNumber number set_1

instance Menge_von Funktion where
   leer                      = []
   vereinige set_1 set_2     | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = set_1 ++ set_2
   schneide set_1 set_2      | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = let ns = [ x | x <- set_1, elem x set_2] in [ y | y <- set_2, elem y ns]
   ziehe_ab set_1 set_2      | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = set_1 \\ set_2
   ist_teilmenge set_1 set_2 | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = sum [ 1 | i <- head (tails set_1), ist_element i set_2] == length set_1
   ist_obermenge set_1 set_2 | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = ist_teilmenge set_2 set_1
   ist_element number set_1  | not (isSet set_1)                      = error "Fehler" | otherwise = countNumber number set_1 >= 1
   ist_leer set_1            | not (isSet set_1)                      = error "Fehler" | otherwise = set_1 == leer
   sind_gleich set_1 set_2   | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = ist_teilmenge set_1 set_2 && ist_teilmenge set_2 set_1
   anzahl number set_1       = countNumber number set_1

{- Knapp, aber gut nachvollziehbar gehen die drei Instanzbildungen fuer Menge_von folgendermassen vor:
   Für jede Menge wird geprüft ob diese wirklich eine gültige Menge ist, also frei von Duplikaten. Ist das nicht der Fall wird der Error "Fehler" ausgegeben.
   Sind die angegebenen Mengen oder die angegebene Menge gültig wird eine Operation auf diesen ausgeführt. Bei der Operation leer wird die Leere Menge zurückgegeben.
   Bei Vereinige werden die beiden angegebenen Mengen miteinander vereinigt. Bei Scheide werden nur die Abbildungen die in der ersten Menge noch nicht enthalten sind
   zur Menge hinzugefügt. Bei ziehe_ab werden die beiden Mengen geschnitten. Bei ist_teilmenge wird geprüft, ob alle Elemente der einen Menge in der anderen liegen.
   ist_obermenge ruft ist_teilmenge auf. ist_leer überprüft, ob die angegebene Menge die leere Menge ist. sind_gleich prüft ob beide Mengen Teilmenge der jeweils anderen
   Menge ist. anzahl gibt die Anzahl einer Nummer in der Menge zurück.
-}

-- Aufgabe A.4
instance (Eq a, Ord a, Eq b, Ord b) => Menge_von (Paar a b) where
  leer                      = []
  vereinige set_1 set_2     | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = set_1 ++ set_2
  schneide set_1 set_2      | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = let ns = [ x | x <- set_1, elem x set_2] in [ y | y <- set_2, elem y ns]
  ziehe_ab set_1 set_2      | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = set_1 \\ set_2
  ist_teilmenge set_1 set_2 | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = sum [ 1 | i <- head (tails set_1), ist_element i set_2] == length set_1
  ist_obermenge set_1 set_2 | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = ist_teilmenge set_2 set_1
  ist_element number set_1  | not (isSet set_1)                      = error "Fehler" | otherwise = countNumber number set_1 >= 1
  ist_leer set_1            | not (isSet set_1)                      = error "Fehler" | otherwise = set_1 == leer
  sind_gleich set_1 set_2   | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = ist_teilmenge set_1 set_2 && ist_teilmenge set_2 set_1
  anzahl number set_1       = countNumber number set_1

instance (Eq a, Ord a) => Menge_von (Baum a) where
  leer                      = []
  vereinige set_1 set_2     | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = set_1 ++ set_2
  schneide set_1 set_2      | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = let ns = [ x | x <- set_1, elem x set_2] in [ y | y <- set_2, elem y ns]
  ziehe_ab set_1 set_2      | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = set_1 \\ set_2
  ist_teilmenge set_1 set_2 | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = sum [ 1 | i <- head (tails set_1), ist_element i set_2] == length set_1
  ist_obermenge set_1 set_2 | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = ist_teilmenge set_2 set_1
  ist_element number set_1  | not (isSet set_1)                      = error "Fehler" | otherwise = countNumber number set_1 >= 1
  ist_leer set_1            | not (isSet set_1)                      = error "Fehler" | otherwise = set_1 == leer
  sind_gleich set_1 set_2   | not (isSet set_1) || not (isSet set_2) = error "Fehler" | otherwise = ist_teilmenge set_1 set_2 && ist_teilmenge set_2 set_1
  anzahl number set_1       = countNumber number set_1

{- Knapp, aber gut nachvollziehbar gehen die beiden Instanzbildungen fuer Menge_von folgendermassen vor:
   Für jede Menge wird geprüft ob diese wirklich eine gültige Menge ist, also frei von Duplikaten. Ist das nicht der Fall wird der Error "Fehler" ausgegeben.
   Sind die angegebenen Mengen oder die angegebene Menge gültig wird eine Operation auf diesen ausgeführt. Bei der Operation leer wird die Leere Menge zurückgegeben.
   Bei Vereinige werden die beiden angegebenen Mengen miteinander vereinigt. Bei Scheide werden nur die Abbildungen die in der ersten Menge noch nicht enthalten sind
   zur Menge hinzugefügt. Bei ziehe_ab werden die beiden Mengen geschnitten. Bei ist_teilmenge wird geprüft, ob alle Elemente der einen Menge in der anderen liegen.
   ist_obermenge ruft ist_teilmenge auf. ist_leer überprüft, ob die angegebene Menge die leere Menge ist. sind_gleich prüft ob beide Mengen Teilmenge der jeweils anderen
   Menge ist. anzahl gibt die Anzahl einer Nummer in der Menge zurück.
-}

-- Aufgabe A.5

instance Eq a => Eq (ElemTyp a) where
  ET a_1 == ET a_2 = a_1 == a_2
  ET a_1 /= ET a_2 = a_1 /= a_2


instance Show a => Show (ElemTyp a) where
  show (ET a) = show a

{- Knapp, aber gut nachvollziehbar gehen die beiden Instanzbildungen fuer Eq und Show folgendermassen vor:
   Die Instanzbildung für Eq überprüft ob die a Werte des ElemTyps gleich sind.
   Die Instanzbildung für show zeigt den a Wert des ElemTyps an.
-}

-- Aufgabe A.6

instance Eq a => Menge_von (ElemTyp a) where
  leer                      = []
  vereinige set_1 set_2     = set_1 ++ set_2
  schneide set_1 set_2      = let ns = [ x | x <- set_1, elem x set_2] in [ y | y <- set_2, elem y ns]
  ziehe_ab set_1 set_2      = set_1 \\ set_2
  ist_teilmenge set_1 set_2 = sum [ 1 | i <- head (tails set_1), ist_element i set_2] == length set_1
  ist_obermenge set_1 set_2 = ist_teilmenge set_2 set_1
  ist_element number set_1  = countNumber number set_1 >= 1
  ist_leer set_1            = set_1 == leer
  sind_gleich set_1 set_2   = ist_teilmenge set_1 set_2 && ist_teilmenge set_2 set_1
  anzahl number set_1       = countNumber number set_1

{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer Menge_von folgendermassen vor:
   Mit dieser Instanzbildung wird der polymorphe Typ ElemTyp zu einer Instanz der Typklasse Menge_von.
   Bei der Operation leer wird die Leere Menge zurückgegeben. bei Vereinige werden die beiden angegebenen Mengen miteinander vereinigt.
   Bei Scheide werden nur die Abbildungen die in der ersten Menge noch nicht enthalten sind zur Menge hinzugefügt.
   Bei ziehe_ab werden die beiden Mengen geschnitten. Bei ist_teilmenge wird geprüft, ob alle Elemente der einen Menge in der anderen liegen.
   ist_obermenge ruft ist_teilmenge auf. ist_leer überprüft, ob die angegebene Menge die leere Menge ist.
   sind_gleich prüft ob beide Mengen Teilmenge der jeweils anderen Menge ist. anzahl gibt die Anzahl einer Nummer in der Menge zurück.
-}

-- Aufgabe A.7

instance (Eq a,Eq b,Eq c,Eq d,Eq e) => Menge_von (PH_ElemTyp a b c d e) where

{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer Menge_von folgendermassen vor:

-}

-- Aufgabe A.8

instance (Eq p,Eq q,Eq r) => Menge_von (PH_ElemTyp' p q r) where

{- Knapp, aber gut nachvollziehbar geht die Instanzbildung fuer Menge_von folgendermassen vor:

-}

main = do
  print("")