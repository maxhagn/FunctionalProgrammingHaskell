module Angabe2 where

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
-}

import Data.Time
import Data.List
import Data.Fixed
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
  show (Vorname firstname) = id firstname

instance Show Nachname where
  show (Nachname lastname) = id lastname

instance Show Person where
  show (P firstname lastname _) = show firstname ++ " " ++ show lastname

-- Aufgabe A.2
isLeap :: Jahr -> Bool
isLeap year | mod year 400 == 0 = True
                | mod year 100 == 0 = False
                | mod year   4 == 0 = True
                | otherwise         = False

getMonthDays :: Monat -> Jahr -> Tag
getMonthDays month year
 |  month == Feb && not (isLeap year) = XXVIII
 |  month == Feb && isLeap year = XXIX
 | (month == Apr || month == Jun || month == Sep || month == Nov) = XXX
 | otherwise = XXXI

getNextHour :: Stunde -> Stunde
getNextHour Zwoelf = Eins
getNextHour hour = succ hour

getNextMonth :: Monat -> Monat
getNextMonth Dez = Jan
getNextMonth month = succ month

hourToNat1 :: Stunde -> Nat1
hourToNat1 hour
  | hour == Eins   = 1
  | hour == Zwei   = 2
  | hour == Drei   = 3
  | hour == Vier   = 4
  | hour == Fuenf  = 5
  | hour == Sechs  = 6
  | hour == Sieben = 7
  | hour == Acht   = 8
  | hour == Neun   = 9
  | hour == Zehn   = 10
  | hour == Elf    = 11
  | hour == Zwoelf = 12

dayHourToNat1 :: VHDS -> Stunde -> VorNachMittag  -> Nat1
dayHourToNat1 quarterHour hour foreAfterNoon
  | foreAfterNoon == VM && quarterHour == Viertel                  = hourToNat1 hour - 1
  | foreAfterNoon == VM && quarterHour == Halb                     = hourToNat1 hour - 1
  | foreAfterNoon == VM && quarterHour == Dreiviertel              = hourToNat1 hour - 1
  | foreAfterNoon == VM && quarterHour == Schlag                   = hourToNat1 hour
  | foreAfterNoon == NM && quarterHour == Viertel                  = hourToNat1 hour + 11
  | foreAfterNoon == NM && quarterHour == Halb                     = hourToNat1 hour + 11
  | foreAfterNoon == NM && quarterHour == Dreiviertel              = hourToNat1 hour + 11
  | foreAfterNoon == NM && quarterHour == Schlag                   = hourToNat1 hour + 12

timePointsToDuration :: Datum -> Uhrzeit -> Kontrollzeitpunkt -> Nat0 -> Nat0
timePointsToDuration (D day1 month1 year1) (U (quarterHour1,hour1,foreAfterNoon1)) (D day2 month2 year2, U (quarterHour2,hour2,foreAfterNoon2)) i
 | day1 == day2 && month1 == month2 && year1 == year2 && hour1 == hour2 && foreAfterNoon1 == foreAfterNoon2 = i
 | hour1 == Zwoelf && foreAfterNoon1 == VM = timePointsToDuration (D day1 month1 year1) (U (quarterHour1,(getNextHour hour1),NM)) (D day2 month2 year2, U (quarterHour2,hour2,foreAfterNoon2)) (i + 1)
 | hour1 == Zwoelf && foreAfterNoon1 == NM && day1 == getMonthDays month1 year1 && month1 == Dez = timePointsToDuration (D I Jan (year1 + 1)) (U (quarterHour1,(getNextHour hour1),VM)) (D day2 month2 year2, U (quarterHour2,hour2,foreAfterNoon2)) (i + 1)
 | hour1 == Zwoelf && foreAfterNoon1 == NM && day1 == getMonthDays month1 year1 && month1 /= Dez = timePointsToDuration (D I (getNextMonth month1) year1) (U (quarterHour1,(getNextHour hour1),VM)) (D day2 month2 year2, U (quarterHour2,hour2,foreAfterNoon2)) (i + 1)
 | hour1 == Zwoelf && foreAfterNoon1 == NM && day1 < getMonthDays month1 year1 = timePointsToDuration (D (succ day1) month1 year1) (U (quarterHour1,(getNextHour hour1),VM)) (D day2 month2 year2, U (quarterHour2,hour2,foreAfterNoon2)) (i + 1)
 | otherwise = timePointsToDuration (D day1 month1 year1) (U (quarterHour1,(getNextHour hour1),foreAfterNoon1)) (D day2 month2 year2, U (quarterHour2,hour2,foreAfterNoon2)) (i + 1)

isValidDate :: Datum -> Bool
isValidDate (D day month year)
 | month == Feb && not (isLeap year) && day > XXVIII = False
 | month == Feb && isLeap year && day > XXIX = False
 | (month == Apr || month == Jun || month == Sep || month == Nov) && day > XXX = False
 | otherwise = True

check2G :: (Person,Kontrollzeitpunkt) -> Kontrollergebnis
check2G ((P firstname lastname (Geimpft (vaccine,count))), controlTime)
 | not (isValidDate (fst controlTime)) = Ungueltig
 | vaccine == JundJ = Einlassen
 | (vaccine == AstraZeneca
 || vaccine == BioNTec
 || vaccine == Moderna
 || vaccine == Sputnik
 || vaccine == Sinovac) && count == Zweimal = Einlassen
 | otherwise = Abweisen

check2G ((P firstname lastname (Genesen)), controlTime)
 | not (isValidDate (fst controlTime)) = Ungueltig
 | otherwise = Einlassen

check2G ((P firstname lastname (Getestet testType date time)), controlTime) = Abweisen
check2G ((P firstname lastname (Udrei), controlTime))                       = Abweisen

check2_5G :: (Person,Kontrollzeitpunkt) -> Kontrollergebnis
check2_5G ((P firstname lastname (Geimpft (vaccine,count))), controlTime)
 | not (isValidDate (fst controlTime)) = Ungueltig
 | vaccine == JundJ = Einlassen
 | (vaccine == AstraZeneca
 || vaccine == BioNTec
 || vaccine == Moderna
 || vaccine == Sputnik
 || vaccine == Sinovac) && count == Zweimal = Einlassen
 | otherwise = Abweisen

check2_5G ((P firstname lastname (Genesen)), controlTime)
 | not (isValidDate (fst controlTime)) = Ungueltig
 | otherwise = Einlassen

check2_5G ((P firstname lastname (Getestet testType date time)), controlTime)
 | not (isValidDate date) || not (isValidDate (fst controlTime)) = Ungueltig
 | testType == PCR     && timePointsToDuration date time controlTime 0 <= 72 = Einlassen
 | testType == Antigen = Abweisen
 | otherwise = Abweisen

check2_5G ((P firstname lastname (Udrei), controlTime))                       = Abweisen


check3G :: (Person,Kontrollzeitpunkt) -> Kontrollergebnis
check3G ((P firstname lastname (Geimpft (vaccine,count))), controlTime)
 | not (isValidDate (fst controlTime)) = Ungueltig
 | vaccine == JundJ = Einlassen
 | (vaccine == AstraZeneca
 || vaccine == BioNTec
 || vaccine == Moderna
 || vaccine == Sputnik
 || vaccine == Sinovac) && count == Zweimal = Einlassen
 | otherwise = Abweisen

check3G ((P firstname lastname (Genesen)), controlTime)
 | not (isValidDate (fst controlTime)) = Ungueltig
 | otherwise = Einlassen

check3G ((P firstname lastname (Getestet testType date time)), controlTime)
 | not (isValidDate date) || not (isValidDate (fst controlTime)) = Ungueltig
 | testType == PCR     && timePointsToDuration date time controlTime 0 <= 72 = Einlassen
 | testType == Antigen && timePointsToDuration date time controlTime 0 <= 24 = Einlassen
 | otherwise = Abweisen

check3G ((P firstname lastname (Udrei), controlTime)) = Abweisen



-- Aufgabe A.2
einzulassen :: (Person,Regel,Kontrollzeitpunkt) -> Kontrollergebnis
einzulassen (person,rule,controlTime)  | rule == ZweiG = check2G (person,controlTime)
                                       | rule == ZweieinhalbG = check2_5G (person,controlTime)
                                       | otherwise     = check3G (person,controlTime)

{- Knapp, aber gut nachvollziehbar geht einzulassen folgendermassen vor:
   Die Funktion nimmt eine Person, eine Regel und einen Kontrollzeitpunkt entgegen und gibt ein Kontrollergebnis zurück.
   Vorerst wird überprüft, welche Regel derzeit aktiv ist, es wird entweder check2G, check2_5G oder check3G ausgeführt.
   check2G weist getestete sowie Personen mit udrei Status zurück.
   check2_5G weist Personen mit udrei Status oder mit Antigen Test getestete Personen zurück.
   check3G weist nur Personen mit udrei Status zurück.
-}

-- Aufgabe A.3
einzulassende :: Einlassbegehrende -> Regel -> Kontrollzeitpunkt -> Einzulassende
einzulassende entryPerson rule controlTime = [ show person | person <- head (tails entryPerson), einzulassen (person,rule,controlTime) == Einlassen ]

{- Knapp, aber gut nachvollziehbar geht einzulassende folgendermassen vor:
   Die Funktion nimmt Einlassbegehrende, eine Regel und einen Kontrollzeitpunkt entgegen und gibt einen Liste von Einzulassenden zurück.
   Dafür werden alle Einlassbegehrende mit Hilfe der Funktion einzulassen überprüft und zur Liste hinzugefügt, sollte einzulassen Einlass zurückgeben.
-}

-- Aufgabe A.4
einzulassende_abzuweisende :: Einlassbegehrende -> Regel -> Kontrollzeitpunkt -> (Einzulassende,Abzuweisende)
einzulassende_abzuweisende entryPerson rule controlTime = ([ show person | person <- head (tails entryPerson), einzulassen (person,rule,controlTime) == Einlassen ],
                                                           [ show person | person <- head (tails entryPerson), einzulassen (person,rule,controlTime) == Abweisen ])

{- Knapp, aber gut nachvollziehbar geht einzulassende_abzuweisende folgendermassen vor:
   Die Funktion nimmt Einlassbegehrende, eine Regel und einen Kontrollzeitpunkt entgegen und gibt eine Liste für Einzulassenden und eine für Abzuweisende Personen zurück.
   Dafür werden alle Einlassbegehrende mit Hilfe der Funktion einzulassen überprüft. Gibt einzulassen Einlassen zurück werden diese Personen den Einzulassenden hinzugefügt.
   Gibt einzulassen Abweisen zurück, werden diese Personen der Abzuweisenden Liste hinzugefügt.
-}

-- Aufgabe A.5
nat1ToString :: Nat1 -> String
nat1ToString number
 | number < 10 = "0" ++ show number
 | otherwise = show number

minToString :: VHDS -> String
minToString  quarterHour
 | quarterHour == Schlag = "00"
 | quarterHour == Viertel = "15"
 | quarterHour == Halb = "30"
 | quarterHour == Dreiviertel = "45"

instance Show Uhrzeit where
 show (U (quarterHour,hour,foreAfterNoon))
  | foreAfterNoon == VM && quarterHour == Schlag        = nat1ToString (hourToNat1 hour) ++ ":"++ minToString quarterHour ++ " Uhr"
  | foreAfterNoon == NM && quarterHour == Schlag        = nat1ToString (hourToNat1 hour + 12) ++ ":"++ minToString quarterHour ++ " Uhr"
  | foreAfterNoon == VM                                 = nat1ToString (hourToNat1 hour-1) ++ ":"++ minToString quarterHour ++ " Uhr"
  | foreAfterNoon == NM                                 = nat1ToString (hourToNat1 hour + 11) ++ ":"++ minToString quarterHour ++ " Uhr"

{- Knapp, aber gut nachvollziehbar geht die Implementierung von show fuer Uhrzeit folgendermassen vor:
   Hier gibt es vier Fallunterscheidungen.
   Ist Vormittag und die Viertelstunde ist Schlag wird die angegebene Stunde ausgegeben.
   Ist Vormittag und die Viertelstunde ist nicht Schlag wird die vorherige Stunde mit der angegebenen Viertelstunge verkettet.
   Ist Nachmittag und die Viertelstunde ist Schlag wird die angegebene Stunde plus 12 Stunden ausgegeben.
   Ist Nachmittag und die Viertelstunde ist nicht Schlag wird die angegebene Stunde plus 11 mit der angegebenen Viertelstunge verkettet.
-}

romanToNat1 :: Tag -> Nat1
romanToNat1 day
 | day == I   = 1
 | day == II   = 2
 | day == III   = 3
 | day == IV   = 4
 | day == V  = 5
 | day == VI  = 6
 | day == VII = 7
 | day == VIII   = 8
 | day == IX   = 9
 | day == X   = 10
 | day == XI    = 11
 | day == XII = 12
 | day == XIII   = 13
 | day == XIV   = 14
 | day == XV   = 15
 | day == XVI   = 16
 | day == XVII  = 17
 | day == XVIII  = 18
 | day == XIX = 19
 | day == XX   = 20
 | day == XXI   = 21
 | day == XXII   = 22
 | day == XXIII    = 23
 | day == XXIV = 24
 | day == XXV   = 25
 | day == XXVI   = 26
 | day == XXVII   = 27
 | day == XXVIII   = 28
 | day == XXIX  = 29
 | day == XXX  = 30
 | day == XXXI = 31

monthToNat1 :: Monat -> Nat1
monthToNat1 month
  | month == Jan = 1
  | month == Feb = 2
  | month == Mar = 3
  | month == Apr = 4
  | month == Mai = 5
  | month == Jun = 6
  | month == Jul = 7
  | month == Aug = 8
  | month == Sep = 9
  | month == Okt = 10
  | month == Nov = 11
  | month == Dez = 12

instance Show Datum where
 show (D day month year)
  | not (isValidDate (D day month year)) = "Datum ungueltig"
  | otherwise = show (romanToNat1 day) ++ "." ++ show (monthToNat1 month) ++ "." ++ show year

{- Knapp, aber gut nachvollziehbar geht die Implementierung von show fuer Datum folgendermassen vor:
   Es wird überprüft, ob das Datum ungueltig ist, wenn das der Fall ist wird der String Datum ungueltig zurückgegeben.
   Ist das Datum gültig, wird der Tag von Roemischen Ziffern zu Nat1 konvertiert und mit Monat und Jahr verkettet.
-}

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

xmas20 :: Kontrollzeitpunkt
xmas20 = (D XXIV Dez 20, U (Schlag, Sechs, NM))

personen :: Einlassbegehrende
personen =
  [ P (Vorname "Donald") (Nachname "Duck") (Geimpft (BioNTec, Einmal)),
    P (Vorname "Dagobert") (Nachname "Duck") (Geimpft (Moderna, Zweimal)),
    P (Vorname "Minnie") (Nachname "Maus") Genesen,
    P (Vorname "Buzz") (Nachname "Lightyear") (Getestet PCR (D XXII Dez 20) (U (Halb, Sechs, NM))),
    P (Vorname "Sheriff") (Nachname "Woody") (Getestet Antigen (D XXIV Dez 20) (U (Schlag, Fuenf, VM)))
  ]

main = do

    print(einzulassende personen DreiG xmas20)
    print(einzulassende personen ZweieinhalbG xmas20)
    print(einzulassende personen ZweiG xmas20)

    print(einzulassende_abzuweisende personen DreiG xmas20)
    print(einzulassende_abzuweisende personen ZweieinhalbG xmas20)
    print(einzulassende_abzuweisende personen ZweiG xmas20)



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
