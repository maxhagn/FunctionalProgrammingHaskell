module Angabe2 where

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
-}

import Data.Time
import Data.List
import Data.Char
import Data.Maybe

-- Aufgabe A.1

-- Ergaenzen Sie fehlende Typklassen in deriving-Klauseln, wo noetig und nicht explizit
-- eine Instanz-Deklaration gefordert ist.


type Nat0              = Int
type Nat1              = Int
newtype Vorname        = Vorname String deriving (Eq,Ord)
newtype Nachname       = Nachname String deriving (Eq,Ord)
data VHDS              = Viertel | Halb | Dreiviertel | Schlag deriving (Eq,Ord,Show)
data Stunde            = Eins | Zwei | Drei | Vier | Fuenf | Sechs
                         | Sieben | Acht | Neun | Zehn | Elf
                         | Zwoelf deriving (Eq,Ord,Show,Enum)
data VorNachMittag     = VM | NM deriving (Eq,Ord,Show)
newtype Uhrzeit        = U (VHDS,Stunde,VorNachMittag) deriving (Eq,Ord)
data Tag               = I | II | III | IV | V | VI | VII | VIII | IX | X
                         | XI | XII | XIII | XIV | XV | XVI | XVII | XVIII
                         | XIX | XX | XXI | XXII | XXIII | XXIV | XXV
                         | XXVI | XXVII | XXVIII | XXIX | XXX
                         | XXXI deriving (Eq,Ord,Show,Enum)
data Monat             = Jan | Feb | Mar | Apr | Mai | Jun
                         | Jul | Aug | Sep | Okt | Nov | Dez deriving (Eq,Ord,Show,Enum)
type Jahr              = Nat1
data Datum             = D Tag Monat Jahr deriving (Eq,Ord)
data Testart           = PCR | Antigen deriving (Eq,Ord,Show)
data Impfstoff         = AstraZeneca | BioNTec | JundJ | Moderna
                         | Sputnik | Sinovac deriving (Eq,Ord,Show)
data Anzahl            = Einmal | Zweimal deriving (Eq,Ord,Show)
data DreiG_Status      = Geimpft (Impfstoff,Anzahl) | Genesen
                         | Getestet Testart Datum Uhrzeit
                         | Udrei deriving (Eq,Ord,Show)
                           -- Udrei: Ungetestet, Ungenesen, Ungeimpft
data Regel             = DreiG | ZweieinhalbG | ZweiG deriving Eq
data Person            = P Vorname Nachname DreiG_Status deriving (Eq,Ord)
type Einlassbegehrende = [Person]
type VorUndNachname    = String
type Einzulassende     = [VorUndNachname]
type Abzuweisende      = [VorUndNachname]
type Kontrollzeitpunkt = (Datum,Uhrzeit)
data Kontrollergebnis  = Einlassen | Abweisen | Ungueltig deriving (Eq,Show)

instance Show Vorname where
  show (Vorname v) = id v

instance Show Nachname where
  show (Nachname n) = id n

instance Show Person where
  show (P vn nn _) = show vn ++ " " ++ show nn

--Hilfsfunktionen

succStunde :: Stunde -> Stunde
succStunde Zwoelf = Eins
succStunde s = succ s

succMonat :: Monat -> Monat
succMonat Dez = Jan
succMonat m = succ m

ist_schaltjahr :: Jahr -> Bool

ist_schaltjahr j | mod j 400 == 0 = True
                 | mod j 100 == 0 = False
                 | mod j 4 == 0 = True
                 | otherwise = False

stundeZuNat1 :: Stunde -> Nat1
stundeZuNat1 s
 | s == Eins = 1
 | s == Zwei = 2
 | s == Drei = 3
 | s == Vier = 4
 | s == Fuenf = 5
 | s == Sechs = 6
 | s == Sieben = 7
 | s == Acht = 8
 | s == Neun = 9
 | s == Zehn = 10
 | s == Elf = 11
 | s == Zwoelf = 12

monatZuNat1 :: Monat -> Nat1
monatZuNat1 m
  | m == Jan = 1
  | m == Feb = 2
  | m == Mar = 3
  | m == Apr = 4
  | m == Mai = 5
  | m == Jun = 6
  | m == Jul = 7
  | m == Aug = 8
  | m == Sep = 9
  | m == Okt = 10
  | m == Nov = 11
  | m == Dez = 12

vhdsZuNat1 :: VHDS -> Nat1
vhdsZuNat1 vhds
  | vhds == Viertel = 1
  | vhds == Halb = 2
  | vhds == Dreiviertel = 3
  | vhds == Schlag = 4


tagZuNat1 :: Tag -> Tag -> Nat1 -> Nat1
tagZuNat1 t ts i | t == ts = i
                   | otherwise = tagZuNat1 t (succ ts) (i+1)

nat1ZuString :: Nat1 -> String
nat1ZuString i
 | i < 10 = "0" ++ show i
 | otherwise = show i

tageInMonat :: Monat -> Jahr -> Tag
tageInMonat m j
 | m == Feb && ist_schaltjahr j = XXIX
 | m == Feb && not (ist_schaltjahr j) = XXVIII
 | (m == Apr || m == Jun || m == Sep || m == Nov) = XXX
 | otherwise = XXXI

datumGueltig :: Datum -> Bool
datumGueltig (D t m j)
 | m == Feb && ist_schaltjahr j && t > XXIX = False
 | m == Feb && not (ist_schaltjahr j) && t > XXVIII = False
 | (m == Apr || m == Jun || m == Sep || m == Nov) && t > XXX = False
 | otherwise = True

datumDiff :: Datum -> Uhrzeit -> Kontrollzeitpunkt -> Nat0 -> Nat0
datumDiff (D tt tm tj) (U (tvhds,ts,tvmnm)) (D t m j, U (vhds,s,vmnm)) i
 | tt == t && tm == m && tj == j && ts == s && tvmnm == vmnm && tvhds == vhds = i
 | tt == t && tm == m && tj == j && ts == s && tvmnm == vmnm && vhdsZuNat1 tvhds < vhdsZuNat1 vhds = i-1
 | tt == t && tm == m && tj == j && ts == s && tvmnm == vmnm && vhdsZuNat1 tvhds > vhdsZuNat1 vhds = i+1
 | ts == Zwoelf && tvmnm == VM = datumDiff (D tt tm tj) (U (tvhds,(succStunde ts),NM)) (D t m j, U (vhds,s,vmnm)) (i + 1) -- change to NM
 | ts == Zwoelf && tvmnm == NM && tt == tageInMonat tm tj && tm == Dez = datumDiff (D I Jan (tj + 1)) (U (tvhds,(succStunde ts),VM)) (D t m j, U (vhds,s,vmnm)) (i + 1) -- Silvester
 | ts == Zwoelf && tvmnm == NM && tt == tageInMonat tm tj && tm /= Dez = datumDiff (D I (succMonat tm) tj) (U (tvhds,(succStunde ts),VM)) (D t m j, U (vhds,s,vmnm)) (i + 1) -- Neues Monat
 | ts == Zwoelf && tvmnm == NM && tt < tageInMonat tm tj = datumDiff (D (succ tt) tm tj) (U (tvhds,(succStunde ts),VM)) (D t m j, U (vhds,s,vmnm)) (i + 1) -- Neuer Tag
 | otherwise = datumDiff (D tt tm tj) (U (tvhds,(succStunde ts),tvmnm)) (D t m j, U (vhds,s,vmnm)) (i + 1) -- Neue Stunde

-- zweiG ->> Keine Tests möglich
zweiG_einzulassen :: (Person,Kontrollzeitpunkt) -> Kontrollergebnis

zweiG_einzulassen ((P vn nn (Geimpft (impfs,anz))), kz)
 | not (datumGueltig (fst kz)) = Ungueltig
 | impfs == Sputnik || impfs == Sinovac = Abweisen
 | impfs == JundJ = Einlassen
 | anz == Zweimal = Einlassen
 | otherwise = Abweisen

zweiG_einzulassen ((P vn nn (Genesen)), kz)
 | not (datumGueltig (fst kz)) = Ungueltig
 | otherwise = Einlassen

zweiG_einzulassen ((P vn nn (Getestet ta d u)), kz) = Abweisen
zweiG_einzulassen ((P vn nn (Udrei), kz)) = Abweisen


-- zweieinhalbG_einzulassen
zweieinhalbG_einzulassen :: (Person,Kontrollzeitpunkt) -> Kontrollergebnis

zweieinhalbG_einzulassen ((P vn nn (Geimpft (impfs,anz))), kz)
 | not (datumGueltig (fst kz)) = Ungueltig
 | impfs == Sputnik || impfs == Sinovac = Abweisen
 | impfs == JundJ = Einlassen
 | anz == Zweimal = Einlassen
 | otherwise = Abweisen

zweieinhalbG_einzulassen ((P vn nn (Genesen)), kz)
 | not (datumGueltig (fst kz)) = Ungueltig
 | otherwise = Einlassen

zweieinhalbG_einzulassen ((P vn nn (Getestet ta d u)), kz)
 | not (datumGueltig d) || not (datumGueltig (fst kz)) = Ungueltig
 | ta == PCR && datumDiff d u kz 0 <= 72 = Einlassen
 | ta == Antigen = Abweisen
 | otherwise = Abweisen

zweieinhalbG_einzulassen ((P vn nn (Udrei), kz)) = Abweisen


-- zweiPlusG ->> PCR nicht älter als 72h, Antigen nicht älter als 24h
zweiPlusG_einzulassen :: (Person,Kontrollzeitpunkt) -> Kontrollergebnis

zweiPlusG_einzulassen ((P vn nn (Geimpft (impfs,anz))), kz)
 | not (datumGueltig (fst kz)) = Ungueltig
 | impfs == Sputnik || impfs == Sinovac = Abweisen
 | impfs == JundJ = Einlassen
 | anz == Zweimal = Einlassen
 | otherwise = Abweisen

zweiPlusG_einzulassen ((P vn nn (Genesen)), kz)
 | not (datumGueltig (fst kz)) = Ungueltig
 | otherwise = Einlassen

zweiPlusG_einzulassen ((P vn nn (Getestet ta d u)), kz)
 | not (datumGueltig d) || not (datumGueltig (fst kz)) = Ungueltig
 | ta == PCR && datumDiff d u kz 0 <= 72 = Einlassen
 | ta == Antigen && datumDiff d u kz 0 <= 24 = Einlassen
 | otherwise = Abweisen

zweiPlusG_einzulassen ((P vn nn (Udrei), kz)) = Abweisen

nameVon :: Person -> VorUndNachname
nameVon (P v n _) = show v ++ " " ++ show n

-- Aufgabe A.2

einzulassen :: (Person,Regel,Kontrollzeitpunkt) -> Kontrollergebnis

einzulassen (p,r,kz)  | r == ZweiG = zweiG_einzulassen (p,kz)
                      | r == ZweieinhalbG = zweieinhalbG_einzulassen (p,kz)
                      | otherwise = zweiPlusG_einzulassen (p,kz)

{- Knapp, aber gut nachvollziehbar geht einzulassen folgendermassen vor:
   Zuerst wird geprüft ob die Regel zweiG oder lockerer ist. Danach wird jeweils
   gecheckt ob es sich um eine Person handelt die genesen, geimpft, getestet oder nichts ist.
   Bei Genesen und geimpft wird einfach Einlassen und Abweisen zurück. Sonst werden die Tests
   Auf Zeit gecheckt.
-}

-- Aufgabe A.3

einzulassende :: Einlassbegehrende -> Regel -> Kontrollzeitpunkt -> Einzulassende

einzulassende eb r kz = [ show p | p <- head (tails eb), einzulassen (p,r,kz) == Einlassen ]

{- Knapp, aber gut nachvollziehbar geht einzulassende folgendermassen vor:
   Hier wird die liste von Personen durchgegangen und für jede Person die einlassen Funktion
   aufgerufen. Das ergebnis wird gecheckt und je nach Ergebnis zur Liste geadded.
-}

-- Aufgabe A.4

einzulassende_abzuweisende :: Einlassbegehrende -> Regel -> Kontrollzeitpunkt -> (Einzulassende,Abzuweisende)

einzulassende_abzuweisende eb r kz = ([ show p | p <- head (tails eb), einzulassen (p,r,kz) == Einlassen ], [ show p | p <- head (tails eb), einzulassen (p,r,kz) == Abweisen ])

{- Knapp, aber gut nachvollziehbar geht einzulassende_abzuweisende folgendermassen vor:
   Ähnlich wie bei Punkt A.3 nur wird hier zusätzlich auf Abgewiesene Personen gecheckt,
   welche separat zur Abgewiesenen-Liste hinzugefügt werden.
-}


-- Aufgabe A.5

instance Show Uhrzeit where
 show (U (vhds,s,vmnm))
  | vmnm == VM && vhds == Schlag && s == Zwoelf = "00:00 Uhr" -- Sonderfall 00:00
  | vmnm == VM && vhds == Viertel = nat1ZuString (stundeZuNat1 s-1) ++ ":15" ++ " Uhr"
  | vmnm == VM && vhds == Halb = nat1ZuString (stundeZuNat1 s-1) ++ ":30" ++ " Uhr"
  | vmnm == VM && vhds == Dreiviertel = nat1ZuString (stundeZuNat1 s-1) ++ ":45" ++ " Uhr"
  | vmnm == VM && vhds == Schlag = nat1ZuString (stundeZuNat1 s) ++ ":00" ++ " Uhr"
  | vmnm == NM && vhds == Viertel = nat1ZuString (stundeZuNat1 s + 11) ++ ":15" ++ " Uhr"
  | vmnm == NM && vhds == Halb = nat1ZuString (stundeZuNat1 s + 11) ++ ":30" ++ " Uhr"
  | vmnm == NM && vhds == Dreiviertel = nat1ZuString (stundeZuNat1 s + 11) ++ ":45" ++ " Uhr"
  | vmnm == NM && vhds == Schlag = nat1ZuString (stundeZuNat1 s + 12) ++ ":00" ++ " Uhr"

{- Knapp, aber gut nachvollziehbar geht die Implementierung von show fuer Uhrzeit
   folgendermassen vor:
   Hier geht es vor allem um Fallunterscheidungen (Vormittag/Nachmittag und
   Minuten). Außerdem muss auf den Sonderfall 00:00 statt 12:00 geachtet werden.
-}


instance Show Datum where
 show (D t m j)
  | not (datumGueltig (D t m j)) = "Datum ungueltig"
  | otherwise = show (tagZuNat1 t I 1) ++ "." ++ show (monatZuNat1 m) ++ "." ++ show j


{- Knapp, aber gut nachvollziehbar geht die Implementierung von show fuer Datum
   folgendermassen vor:
   Beim Datum werden einfach Funktionen verkettet, welche Tag und Monat zu einem
   integer bzw. String machen und Verketten.
-}

-- Testcases
ta = PCR :: Testart
dgs1 = Getestet PCR (D XX Okt 2021) (U (Viertel,Acht,VM)) :: DreiG_Status
dgs2 = Geimpft (BioNTec,Einmal) :: DreiG_Status
dgs3 = Genesen :: DreiG_Status
dgs4 = Getestet Antigen (D XXII Sep 2021) (U (Schlag,Acht,VM)) :: DreiG_Status
buergermeister = P (Vorname "Michael") (Nachname "Ludwig") dgs1 :: Person
bundesminister = P (Vorname "Wolfgang") (Nachname "Mueckstein") dgs2 :: Person
bundeskanzler = P (Vorname "Alexander") (Nachname "Schallenberg") dgs3 :: Person
bundespraesident = P (Vorname "Alexander") (Nachname "van der Bellen") dgs4 :: Person
bgm = buergermeister
bm = bundesminister
bk = bundeskanzler
bp = bundespraesident
kzp1 = ((D XXII Okt 2021), (U (Dreiviertel,Acht,NM)))
kzp2 = ((D XXVIII Okt 2021), (U (Dreiviertel,Acht,NM)))
kzp3 = ((D XXXI Nov 2021), (U (Dreiviertel,Acht,NM)))


main = do
    print("Aufgabe A.2")
    print(einzulassen (bgm,DreiG,kzp1)) -- Einlassen
    print(einzulassen (bgm,DreiG,kzp2)) -- Abweisen
    print(einzulassen (bgm,DreiG,kzp3)) -- Ungueltig
    print(einzulassen (bm,DreiG,kzp1)) -- Abweisen
    print(einzulassen (bm,DreiG,kzp2)) -- Abweisen
    print(einzulassen (bm,DreiG,kzp3)) -- Ungueltig
    print(einzulassen (bk,DreiG,kzp1)) -- Einlassen
    print(einzulassen (bk,DreiG,kzp2)) -- Einlassen
    print(einzulassen (bk,DreiG,kzp3)) -- Ungueltig
    print(einzulassen (bp,ZweieinhalbG,kzp1)) -- Abweisen
    print(einzulassen (bp,ZweieinhalbG,kzp2)) -- Abweisen
    print(einzulassen (bp,ZweieinhalbG,kzp3)) -- Ungueltig

    print("Aufgabe A.3")
    print(einzulassende [bgm,bm,bk,bp] DreiG kzp1)

    print("Aufgabe A.4")
    print(einzulassende_abzuweisende [bgm,bm,bk,bp] DreiG kzp1)

    print("Aufgabe A.5")
    print(show (U (Viertel,Zwoelf,VM))) -- "11:15 Uhr"
    print(show (U (Viertel,Zwoelf,NM))) -- "23:15 Uhr"
    print(show (U (Dreiviertel,Zwoelf,VM))) -- "11:45 Uhr"
    print(show (U (Dreiviertel,Zwoelf,NM))) -- "23:45 Uhr"
    print(show (U (Schlag,Zwoelf,VM))) -- "00:00 Uhr"
    print(show (U (Schlag,Zwoelf,NM))) -- "24:00 Uhr"
    print(show (U (Halb,Sechs,VM))) -- "05:30 Uhr"
    print(show (U (Halb,Sechs,NM))) -- "17:30 Uhr"
    print(show (D XXII Okt 2021)) -- "22.10.2021"
    print(show (D XXIV Dez 2412))-- "24.12.2412"
    print(show (D XXXI Feb 1234)) -- "Datum ungueltig"
