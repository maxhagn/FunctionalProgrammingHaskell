> module Angabe4 where

1. Vervollstaendigen Sie gemaess Angabentext!
2. Vervollst�ndigen Sie auch die vorgegebenen Kommentaranf�nge!
3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!

> import Prelude
> import Data.List
> import Data.Char
> import Data.Ord
> import Data.Function (on)


Datenstrukturen fuer eine einfache Buchhaltung:

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

> instance Num Cent where
>  C c1 + C c2       = C (c1 + c2)
>  C c1 - C c2       = C (c1 - c2)
>  C c1 * C c2       = C (c1 * c2)
>  abs c             = c
>  signum c          = c
>  fromInteger c     = C (fromIntegral c)


> type Nat0    = Int
> skontoInCent :: Skonto -> Cent -> Cent
> skontoInCent s (C c) | s == KeinSkonto = C 0
>                      | s == DreiProzent = C ((3 * c) `quot` 100)
>                      | s == FuenfProzent = C ((5 * c) `quot` 100)
>                      | s == ZehnProzent = C ((10 * c) `quot` 100)

> ist_schaltjahr :: Jahr -> Bool
> ist_schaltjahr j | mod j 400 == 0 = True
>                  | mod j 100 == 0 = False
>                  | mod j 4 == 0 = True
>                  | otherwise = False

> succMonat :: Monat -> Monat
> succMonat Dez = Jan
> succMonat m = succ m

> datumGueltig :: Datum -> Datum
> datumGueltig (D t m j)
>  | m == Feb && ist_schaltjahr j && t > XXIX = (D I Mar j)
>  | m == Feb && not (ist_schaltjahr j) && t > XXVIII = (D I Mar j)
>  | (m == Apr || m == Jun || m == Sep || m == Nov) && t > XXX = (D I (succMonat m) j)
>  | otherwise = (D t m j)

> waup :: Kassabucheintrag -> AP_Kassabucheintrag
> waup (GP n d1, Zahlung b s d2) = (GP n (datumGueltig d1), AP_Zahlung (b - (skontoInCent s b)) (datumGueltig d2))
> waup (GP n d1, Gutschrift b d2) = (GP n (datumGueltig d1), P_Gutschrift b (datumGueltig d2))

Knapp, aber gut nachvollziehbar geht waup folgendermassen vor:
In jedem Fall wird das Datum des Geschäftspartners durch datumGueltig ausgebessert. Bei Zahlungen wird der Skonto ueber skontoInCent berechnet und abgezogen
(instanz Num Cent) und ebenfalls das Datum der Transaktion bereinigt. Im Gutschrift Fall wird die Gutschrift direkt übernommen und das Datum bereinigt.


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
> centsToEuroCent (C c) | c < 0 = EC (c `quot` 100) (c `mod` (-100))
>                       | otherwise = EC (c `quot` 100) (c `mod` 100)

> eintragKonsolidieren :: Kassabucheintrag -> (P_Geschaeftspartner, K_Geschaeftsvorfall)
> eintragKonsolidieren (GP n d1, Zahlung b s d2) = (GP n (datumGueltig d1), K_Zahlung (centsToEuroCent (b - (skontoInCent s b))) (datumGueltig d2))
> eintragKonsolidieren (GP n d1, Gutschrift b d2) = (GP n (datumGueltig d1), K_Gutschrift (centsToEuroCent b) (datumGueltig d2))

> konsolidiere :: Kassabuch -> KonsolidiertesKassabuch
> konsolidiere (KB kb) = KKB [ eintragKonsolidieren e | e <- head (tails kb), True]

Knapp, aber gut nachvollziehbar geht konsolidiere folgendermassen vor:
Beim Konsolidieren werden die Kassabucheintraege durchgegangen und ein neuer Array aus Tupeln fuer das Kons. Kassabuch erstellt.
Dort wird aehnlich A.1 vorgegangen (und auf EurCent umgerechnet).


Aufgabe A.3

> data Saldo = Forderungssaldo { fs :: EuroCent }
>              | Zahlungssaldo { zs :: EuroCent }
>              | Ausgeglichen
>              | Keine_Geschaeftsbeziehung deriving (Eq,Show)

> gvIsZahlung :: K_Geschaeftsvorfall -> Bool
> gvIsZahlung (K_Zahlung _ _) = True
> gvIsZahlung _     = False

> instance Num EuroCent where
>  EC e1 c1 + EC e2 c2       = centsToEuroCent(C ((e1*100) + (e2*100) + c1 + c2))
>  EC e1 c1 - EC e2 c2       = centsToEuroCent(C ((e1*100) - (e2*100) + c1 - c2))
>  EC e1 c1 * EC e2 c2       = centsToEuroCent(C (((e1*100) + c1) * ((e2*100) + c2)))
>  negate (EC e1 c1)         = (EC (e1 * (-1)) (c1 * (-1)))
>  abs ec                    = ec
>  signum ec                 = ec
>  fromInteger c             = centsToEuroCent (C (fromIntegral c))

> saldo :: P_Geschaeftspartner -> KonsolidiertesKassabuch -> Saldo
> saldo (GP n d1) (KKB kkb) | sum [1 | e <- head (tails kkb), partner (fst e) == n && seit (fst e) == d1] == 0 = Keine_Geschaeftsbeziehung
>                           | sum [ if gvIsZahlung (snd e) then ec_netto (snd e) else negate (ec_gutschrift (snd e)) | e <- head (tails kkb), partner (fst e) == n && seit (fst e) == d1] < 0 =
>                               Forderungssaldo (sum [ if gvIsZahlung (snd e) then negate (ec_netto (snd e)) else ec_gutschrift (snd e) | e <- head (tails kkb), partner (fst e) == n && seit (fst e) == d1])
>                           | sum [ if gvIsZahlung (snd e) then ec_netto (snd e) else negate (ec_gutschrift (snd e)) | e <- head (tails kkb), partner (fst e) == n && seit (fst e) == d1] > 0 =
>                               Zahlungssaldo (sum [ if gvIsZahlung (snd e) then ec_netto (snd e) else negate (ec_gutschrift (snd e)) | e <- head (tails kkb), partner (fst e) == n && seit (fst e) == d1])
>                           | sum [ if gvIsZahlung (snd e) then ec_netto (snd e) else negate (ec_gutschrift (snd e)) | e <- head (tails kkb), partner (fst e) == n && seit (fst e) == d1] == 0 =
>                               Ausgeglichen
>                           | otherwise = error "i dont know"

Knapp, aber gut nachvollziehbar geht saldo folgendermassen vor:
Hier wird die summe der Geschäftsfaelle des Geschaeftspartners gebildet. Dann die Fallunterscheidung für Forderung/Zahlung/Ausgeglichen.
Je nach Fall wird unterschieden welche Art von Geschaeftsvorfall negativ und welche positiv zu betrachten ist.


Aufgabe A.4

> newtype SaldiertesKassabuch = SKB [(Geschaeftspartner,Saldo)] deriving (Show)

> konsolidiereArray :: Kassabuch -> [(P_Geschaeftspartner, K_Geschaeftsvorfall)]
> konsolidiereArray (KB kb) = [ eintragKonsolidieren e | e <- head (tails kb), True]

> rmdups :: (Eq a, Eq b) => [(a,b)] -> [(a,b)]
> rmdups [] = []
> rmdups (x:xs) = x : rmdups (filter (\y -> not(x == y)) xs)

> saldiere :: Kassabuch -> SaldiertesKassabuch
> saldiere (KB kb) = SKB (Data.List.sortOn fst (rmdups [ (fst e, saldo (fst e) (konsolidiere (KB kb))) | e <- head (tails  (konsolidiereArray (KB kb))), True]))

Knapp, aber gut nachvollziehbar geht saldiere folgendermassen vor:
Hier wird für jeden Eintrag des Kassabuchs (für jeden Geschaeftspartner) einmal die Funktion aus A.3 ausgeführt.
Anschließend wird die Liste bereinigt (entfernen von doppelten eintraegen) und sortiert (sortOn)



-- ----------------------------------------------------------------------------
-- Testdaten, Geschäftspartner
-- ----------------------------------------------------------------------------

> mattBerry = GP "Matt Berry" (D XX Jun 2015)

> nadiaCitrus = GP "Nadia Citrus" (D XXIX Feb 2000)

> jasonFruit = GP "Jason Fruit" (D XXXI Jun 2014)

> jasonFruit_AP = GP "Jason Fruit" (D I Jul 2014)

-- ----------------------------------------------------------------------------
-- Testdaten, Geschaeftsvorfall
-- ----------------------------------------------------------------------------

> ormaleZahlung = Zahlung (C 5000) KeinSkonto endeJuni

> normaleZahlung_AP = AP_Zahlung (C 5000) endeJuni

> dreiProzentSkontoZahlung = Zahlung (C 60000) DreiProzent endeJuni31

> dreiProzentSkontoZahlung_AP = AP_Zahlung (C 58200) anfangJuli

> fuenfProzentSkontoZahlung = Zahlung (C 5831245) FuenfProzent anfangJuli

> fuenfProzentSkontoZahlung_AP = AP_Zahlung (C 5539683) anfangJuli

> zehnProzentSkontoZahlung = Zahlung (C 5831245) ZehnProzent anfangJuli

> zehnProzentSkontoZahlung_AP = AP_Zahlung (C 5248121) anfangJuli

> gutschriftDatumUngueltig = Gutschrift (C 1000) endeFebruar

> gutschriftDatumUngueltig_AP = P_Gutschrift (C 1000) anfangMaerz

> endeJuni = D XXX Jun 2016

> endeJuni31 = D XXXI Jun 2016

> anfangJuli = D I Jul 2016

> endeFebruar = D XXIX Feb 2017

> anfangMaerz = D I Mar 2017

-- ----------------------------------------------------------------------------
-- Testdaten, Hilfsfunktionen
-- ----------------------------------------------------------------------------

> sortSKB (SKB xs) = SKB (sortOn (partner . fst) xs)

> sortKKB (KKB xs) =
>   KKB
>     ( sortBy
>         (comparing (partner . fst) <> (datumComp `on` (datumAPGv . snd)))
>         xs
>     )

> datumAPGv a@K_Zahlung {} = zahlungsdatum' a
> datumAPGv a@K_Gutschrift {} = gutschriftsdatum' a

> datumComp d1 d2 = case compare (jahr d1) (jahr d2) of
>   EQ -> case compare (monat d1) (monat d2) of
>     EQ -> compare (tag d1) (tag d2)
>     x -> x
>   x -> x





> main = do
>    print("Aufgabe A.1")
>    print(waup ((GP "Fritz" (D XXX Feb 2000)), (Gutschrift (C 100) (D XX Jul 2001))))
>    print(waup ((GP "Fritz" (D X Feb 2000)), (Zahlung (C 100) FuenfProzent (D XXXI Sep 2001))))

>    print("Aufgabe A.2")
>    print(konsolidiere (KB [((GP "Fritz" (D XXX Feb 2000)), (Zahlung (C 1337) FuenfProzent (D XXXI Sep 2001))),
>                           ((GP "Fritz" (D XXX Feb 2000)), (Zahlung (C 1099) KeinSkonto (D XXXI Sep 2001))),
>                           ((GP "Fritz" (D XXX Feb 2000)), (Gutschrift (C 1337) (D XXXI Sep 2001)))]))

>    print("Aufgabe A.3")
>    print(saldo (GP "Fritz" (D XXX Feb 2000)) (KKB [((GP "Mani" (D XXX Feb 2000)), (K_Zahlung (EC 20 99) (D XXXI Sep 2001))),
>                           ((GP "Fritz" (D XXX Feb 2000)), (K_Zahlung (EC 10 99) (D XXXI Sep 2001))),
>                           ((GP "Fritz" (D XXX Feb 2000)), (K_Gutschrift (EC 13 37) (D XXXI Sep 2001)))]))

>    print("Aufgabe A.4")
>    print(saldiere (KB [((GP "Fritz" (D XXX Feb 2000)), (Zahlung (C 1337) KeinSkonto (D XXXI Sep 2001))),
>                           ((GP "Fritz" (D XXX Feb 2000)), (Zahlung (C 1099) KeinSkonto (D XXXI Sep 2001))),
>                           ((GP "Fritz" (D XXX Feb 2000)), (Gutschrift (C 1337) (D XXXI Sep 2001))),
>                           ((GP "Fritz" (D XXX Feb 2000)), (Gutschrift (C 1337) (D XXXI Sep 2001))),
>                           ((GP "Fritz" (D XXX Feb 2001)), (Gutschrift (C 1050) (D XXXI Sep 2001))),
>                           ((GP "Fritz" (D XXX Feb 2001)), (Zahlung (C 2000) KeinSkonto (D XXXI Sep 2001))),
>                           ((GP "Fritz" (D XXX Feb 2001)), (Zahlung (C 550) KeinSkonto (D XXXI Sep 2001))),
>                           ((GP "Aritz" (D XXX Feb 2001)), (Gutschrift (C 750) (D XXXI Sep 2001)))]))

>    print()
>    print()
>    print()
>    print()
>    print()
>    print()

>    print(sortSKB (saldiere
>               ( KB
>                   [ (mattBerry, Zahlung (C 1000) KeinSkonto endeJuni),
>                     (mattBerry, Gutschrift (C 1049) endeFebruar)
>                   ]
>               )))
>    print()
>    print(sortSKB ( saldiere
>               ( KB
>                   [ (mattBerry, Zahlung (C 1000) KeinSkonto endeJuni),
>                     (mattBerry, Gutschrift (C 1049) endeFebruar),
>                     (jasonFruit_AP, zehnProzentSkontoZahlung),
>                     (jasonFruit_AP, fuenfProzentSkontoZahlung),
>                     (nadiaCitrus, Zahlung (C 500) KeinSkonto endeJuni),
>                     (nadiaCitrus, Gutschrift (C 500) endeJuni)
>                   ]
>               )))
