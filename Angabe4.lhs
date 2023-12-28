> module Angabe4 where

1. Vervollstaendigen Sie gemaess Angabentext!
2. Vervollst�ndigen Sie auch die vorgegebenen Kommentaranf�nge!
3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat! 

> import Prelude
> import Data.List
> import Data.Char
> import Data.Ord
> import Data.Maybe

Datenstrukturen fuer eine einfache Buchhaltung:

> type Nat0    = Int
> type Nat1    = Int
> type Name    = String
> newtype Cent = C { cents :: Nat1
>                  } deriving (Eq,Ord,Show)
> type Brutto  = Cent
> type Netto   = Cent
> data Skonto  = KeinSkonto | DreiProzent  
>                | FuenfProzent | ZehnProzent deriving (Eq,Ord,Show)
> data Tag     = I | II | III | IV | V | VI | VII | VIII | IX | X
>                | XI | XII | XIII | XIV | XV | XVI | XVII | XVIII 
>                | XIX | XX | XXI | XXII | XXIII | XXIV | XXV
>                | XXVI | XXVII | XXVIII | XXIX | XXX 
>                | XXXI deriving (Eq,Ord,Show)
> data Monat   = Jan | Feb | Mar | Apr | Mai | Jun | Jul | Aug
>                | Sep | Okt | Nov | Dez deriving (Eq,Ord,Show,Enum)
> type Jahr    = Nat1
> data Datum   = D { tag   :: Tag,
>                    monat :: Monat,
>                    jahr  :: Jahr 
>                  } deriving (Eq,Show,Ord)

> data Geschaeftspartner = GP { partner :: Name,
>                               seit :: Datum
>                              } deriving (Eq,Show,Ord)
> data Geschaeftsvorfall = Zahlung { brutto :: Brutto,
>                                    skonto :: Skonto,
>                                    zahlung_vom :: Datum
>                                  }
>                          | Gutschrift { gutschriftsbetrag :: Cent,
>                                         gutschrift_vom :: Datum
>                                       } 
>                          deriving (Eq,Show)
> type Kassabucheintrag = (Geschaeftspartner,Geschaeftsvorfall)
> newtype Kassabuch = KB [Kassabucheintrag] deriving (Eq,Show)

> instance Num Cent where
>  C cent_1 + C cent_2 = C (cent_1 + cent_2)
>  C cent_1 - C cent_2 = C (cent_1 - cent_2)
>  C cent_1 * C cent_2 = C (cent_1 * cent_2)
>  fromInteger cent    = C (fromIntegral cent)
>  abs cent              | cent >= 0 = cent
>                        | otherwise = (-1) * cent
>  signum cent          = cent

> instance Num EuroCent where
>  EC euro_1 cent_1 + EC euro_2 cent_2 = centsToEuroCent(C ((euro_1*100) + (euro_2*100) + cent_1 + cent_2))
>  EC euro_1 cent_1 - EC euro_2 cent_2 = centsToEuroCent(C ((euro_1*100) - (euro_2*100) + cent_1 - cent_2))
>  EC euro_1 cent_1 * EC euro_2 cent_2 = centsToEuroCent(C (((euro_1*100) + cent_1) * ((euro_2*100) + cent_2)))
>  negate (EC euro cent)               = (EC (euro * (-1)) (cent * (-1)))
>  fromInteger cent                    = centsToEuroCent (C (fromIntegral cent))
>  abs (EC euro cent)                  = (EC (euro) (cent))
>  signum (EC euro cent)               = (EC (euro) (cent))

Aufgabe A.1

> type P_Geschaeftspartner = Geschaeftspartner
> data AP_Geschaeftsvorfall 
>    = AP_Zahlung { netto :: Netto,
>                   zahlungsdatum :: Datum
>                 }
>     | P_Gutschrift { gutschrift :: Cent,
>                      gutschriftsdatum :: Datum
>                    } deriving (Eq,Show)   
> type AP_Kassabucheintrag = (P_Geschaeftspartner,AP_Geschaeftsvorfall)

> getNextMonth :: Monat -> Monat
> getNextMonth Dez = Jan
> getNextMonth m = succ m

> isLeap :: Jahr -> Bool
> isLeap year | mod year 400 == 0 = True
>             | mod year 100 == 0 = False
>             | mod year   4 == 0 = True
>             | otherwise         = False

> getValidDate :: Datum -> Datum
> getValidDate (D day month year)
>  | month == Feb && not (isLeap year) && day > XXVIII = (D I Mar year)
>  | month == Feb && isLeap year && day > XXIX = (D I Mar year)
>  | (month == Apr || month == Jun || month == Sep || month == Nov) && day > XXX = (D I (getNextMonth month) year)
>  | (month == Jan || month == Mar || month == Mai || month == Jul || month == Aug || month == Okt || month == Dez) && day > XXXI = (D I (getNextMonth month) year)
>  | otherwise = (D day month year)

> discountToCent :: Skonto -> Cent -> Cent
> discountToCent discount (C cent) | discount == KeinSkonto   = C 0
>                                  | discount == DreiProzent  = C ((3  * cent) `quot` 100)
>                                  | discount == FuenfProzent = C ((5  * cent) `quot` 100)
>                                  | discount == ZehnProzent  = C ((10 * cent) `quot` 100)

> waup :: Kassabucheintrag -> AP_Kassabucheintrag
> waup (GP partner since, Zahlung brutto discount date) = (GP partner (getValidDate since), AP_Zahlung (brutto - (discountToCent discount brutto)) (getValidDate date))
> waup (GP partner since, Gutschrift brutto date)       = (GP partner (getValidDate since), P_Gutschrift brutto (getValidDate date))

Knapp, aber gut nachvollziehbar geht waup folgendermassen vor: 
Die Funktion nimmt einen Kassabucheintrag entgehen und gibt einen AP_Kassabucheintrag, also einen ausgewerteten und plausiblisierten Kassabucheintrag zurück.
Es wird unterschieden, ob der Geschäftsvorfall von Typ Zahlung oder Gutschrift ist. In beiden Fällen wird überprüft ob das Datum des Geschäftspartners plausibel ist.
GetValidDate überprüft, ob das Datum gültig ist. Falls dieses nicht gültig ist, wird das nächste gültige Datum zurückgegeben. discountToCent berechnet den Skonto in
cent, je nach angegebenem Prozentsatz.
Cent.

Aufgabe A.2

> data EuroCent = EC { euro :: Nat1,
>                      cent :: Nat1

                       Nur Werte zwischen 0 und 99 fuer cent!

>                    } deriving (Eq,Ord,Show)

> data K_Geschaeftsvorfall = K_Zahlung { ec_netto :: EuroCent,
>                                        zahlungsdatum' :: Datum
>                                      }
>                            | K_Gutschrift { ec_gutschrift :: EuroCent,
>                                             gutschriftsdatum' :: Datum
>                                           } deriving (Eq,Show)

> newtype KonsolidiertesKassabuch
>     = KKB [(P_Geschaeftspartner,K_Geschaeftsvorfall)]
>       deriving (Eq,Show)

> centsToEuroCent :: Cent -> EuroCent
> centsToEuroCent (C cent) | cent < 0  = EC (cent `quot` 100) (cent `mod` (-100))
>                          | otherwise = EC (cent `quot` 100) (cent `mod` 100)

> consolidateEntry :: Kassabucheintrag -> (P_Geschaeftspartner, K_Geschaeftsvorfall)
> consolidateEntry (GP partner since, Zahlung brutto discount date) = (GP partner (getValidDate since), K_Zahlung    (centsToEuroCent (brutto - (discountToCent discount brutto))) (getValidDate date))
> consolidateEntry (GP partner since, Gutschrift brutto date)       = (GP partner (getValidDate since), K_Gutschrift (centsToEuroCent brutto) (getValidDate date))

> konsolidiere :: Kassabuch -> KonsolidiertesKassabuch
> konsolidiere (KB cashBook) = KKB [ consolidateEntry cashEntry | cashEntry <- head (tails cashBook), True]

Knapp, aber gut nachvollziehbar geht konsolidiere folgendermassen vor:
Die Funktion nimmt ein Kassabuch entgehen und gibt ein KonsolidiertesKassabuch zurück. Für jeden Eintrag in Kassabuch wird die Funktion consolidateEntry aufgerufen.
Es werden wieder alle Daten auf Gültigkeit geprüft und bei Ungültigkeit, das nächste gültige Datum eingetragen. Es wird wieder der Nettobetrag ausgerechnet und diesmal in
Euro und Cent umgerechnet.


Aufgabe A.3

> data Saldo = Forderungssaldo { fs :: EuroCent }
>              | Zahlungssaldo { zs :: EuroCent }
>              | Ausgeglichen
>              | Keine_Geschaeftsbeziehung deriving (Eq,Show)

> isPayment :: K_Geschaeftsvorfall -> Bool
> isPayment (K_Zahlung _ _) = True
> isPayment _               = False

> saldo :: P_Geschaeftspartner -> KonsolidiertesKassabuch -> Saldo
> saldo (GP partner_name since) (KKB cashBook) | sum [ 1 | entry <- head (tails cashBook), partner (fst entry) == partner_name && seit (fst entry) == since] == 0 =
>                                                    Keine_Geschaeftsbeziehung
>                                              | sum [ if isPayment (snd entry) then ec_netto (snd entry) else negate (ec_gutschrift (snd entry))
>                                                    | entry <- head (tails cashBook), partner (fst entry) == partner_name && seit (fst entry) == since] < 0 =
>                                                    Forderungssaldo
>                                                    (sum [ if isPayment (snd entry) then negate (ec_netto (snd entry)) else ec_gutschrift (snd entry)
>                                                    | entry <- head (tails cashBook), partner (fst entry) == partner_name && seit (fst entry) == since])
>                                              | sum [ if isPayment (snd entry) then ec_netto (snd entry) else negate (ec_gutschrift (snd entry))
>                                                    | entry <- head (tails cashBook), partner (fst entry) == partner_name && seit (fst entry) == since] > 0 =
>                                                    Zahlungssaldo
>                                                    (sum [ if isPayment (snd entry) then ec_netto (snd entry) else negate (ec_gutschrift (snd entry))
>                                                    | entry <- head (tails cashBook), partner (fst entry) == partner_name && seit (fst entry) == since])
>                                              | sum [ if isPayment (snd entry) then ec_netto (snd entry) else negate (ec_gutschrift (snd entry))
>                                                    | entry <- head (tails cashBook), partner (fst entry) == partner_name && seit (fst entry) == since] == 0 =
>                                                    Ausgeglichen


Knapp, aber gut nachvollziehbar geht saldo folgendermassen vor:
Die Funktion nimmt einen Geschäftspartner und ein Konsolidiertes Kassabuch entgehen. Zuerst wird überprüft, ob Einträge zu dem angegebenen Geschäftspartner
verfügbar sind. Ist dies nicht der Fall wird Keine_Geschäftsbeziehung retourniert. Gibt es Einträge und übersteigt die Summe der Gutschriften die Summe der Zahlungen
so wird Forderungssaldo und der dazugehörige Wert retourniert. Übersteigen die Zahlungen die Gutschriften so wird Zahlungssaldo und der dazugehörige Wert retourniert.
Ist die Summe der Zahlungen und die Summe der Gutschriften ausgeglichen wird Ausgeglichen retourniert.



Aufgabe A.4

> newtype SaldiertesKassabuch = SKB [(Geschaeftspartner,Saldo)] deriving (Show)

> consolidateCashBook :: Kassabuch -> [(P_Geschaeftspartner, K_Geschaeftsvorfall)]
> consolidateCashBook (KB cashBook) = [ consolidateEntry entry | entry <- head (tails cashBook), True]

> removeDublicates :: (Eq entry_1, Eq entry_2) => [(entry_1,entry_2)] -> [(entry_1,entry_2)]
> removeDublicates [] = []
> removeDublicates (x:xs) = x : removeDublicates (filter (\y -> not(x == y)) xs)

> saldiere :: Kassabuch -> SaldiertesKassabuch
> saldiere (KB cashBook) = SKB (Data.List.sortOn fst (removeDublicates [ (fst entry, saldo (fst entry) (konsolidiere (KB cashBook)))
>                                                                      | entry <- head (tails  (consolidateCashBook (KB cashBook))), True]))


Knapp, aber gut nachvollziehbar geht saldiere folgendermassen vor:
Die Funktion nimmt ein Kassabuch entgegen und retouniert ein Saldiertes Kassabuch.
Vorerst wird das angegebene Kassabuch konsoldiert und anschließend
über jeden Eintrag iteriert. Die Dublikate vom konsoldierten Kassabuch werden entfernt
und anschließend wird das Array nach den Geschäftspartners sortiert.


> main = do
>    print("")