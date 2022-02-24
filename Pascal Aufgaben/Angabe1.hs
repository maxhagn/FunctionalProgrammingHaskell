module Angabe1 where

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
-}

import Data.List
import Data.Char
import Data.Maybe

type Nat0                = Int
type Zeichenreihe        = String
type Teilzeichenreihe    = String
type IstTeilzeichenreihe = Bool
type Zerlegungszeuge     = (Zeichenreihe,Zeichenreihe,Zeichenreihe)
type Zerlegungszeugen    = [Zerlegungszeuge]


-- Aufgabe A.1

ist_tzr :: Zeichenreihe -> Teilzeichenreihe -> IstTeilzeichenreihe

ist_tzr zr tzr = isInfixOf tzr zr

{- Knapp, aber gut nachvollziehbar geht ist_tzr folgendermassen vor:
   Die funktion nimmt eine Zeichenreihe und eine Reihe und überprüft mit isInfixOf ob die tzr in der zr vorkommt.
-}

-- Aufgabe A.2

tzr_zeuge :: Zeichenreihe -> Teilzeichenreihe -> Zerlegungszeuge

findString :: (Eq a) => [a] -> [a] -> Int
findString search str = fromMaybe (-1) $ findIndex (isPrefixOf search) (tails str)

substring :: Int -> Int -> String -> String
substring i j s = take (j-i) ( drop i s )

tzr_zeuge zr tzr | findString tzr zr < 0 = ("", tzr ++ tzr, "")
                 | otherwise = ((substring 0 (findString tzr zr) zr), tzr, (substring ((findString tzr zr) + length tzr) (length zr) zr))

{- Knapp, aber gut nachvollziehbar geht tzr_zeuge folgendermassen vor:
   Die Funktion nimmt eine Zeichenreihe und eine Teilzeichenreihe und prüft,
   ob überhauot die tzr in zr enthalten ist. Wenn nicht dann Sonderfall ("", tt, "")!
   Ansonsten werden substrings vor und nach dem vorkommenden Wort gebildet.
   Das vorkommende Wort wird mit findString ermittelt, welche den Index des ersten vorkommens zurückliefert.
-}

-- Aufgabe A.3

tzr_zeugen :: Zeichenreihe -> Teilzeichenreihe -> Zerlegungszeugen

tzr_zeugen zr tzr = [ ((substring 0 (length zr - length r) zr), tzr, (substring (length zr - length r + length tzr) (length zr) zr)) | r <- tails zr, isPrefixOf tzr r ]

{- Knapp, aber gut nachvollziehbar geht tzr_zeugen folgendermassen vor:
   Hier wird ein Array in der Form von Zerlegungszeugen erstellt, wenn der nächstkleinere Teilstring
   die tzr an der ersten Stelle enthält.
-}


-- Aufgabe A.4

wieOft :: Zeichenreihe -> Teilzeichenreihe -> Nat0

wieOft zr tzr = sum [ 1 | r <- tails zr, isPrefixOf tzr r ]

{- Knapp, aber gut nachvollziehbar geht wieOft folgendermassen vor:
   Hier wird ein Array mit 1ern gefüllt, immer wenn der nächstkleinere Teilstring tzr an der ersten
   Stelle enthält. Anschließend wird die Summe zurückgeliefert.
-}



-- Testcases
main = do
    print ("--Aufgabe A.1--")
    print (ist_tzr "Urknallexplosion" "knall")
    print (ist_tzr "Urknallexplosion" "Knall")
    print (ist_tzr "Urknallexplosion" "")
    print (ist_tzr "" "Urknallexplosion")
    print (ist_tzr "Urknallexplosionsknall" "knall")
    putStrLn ""

    print("--Aufgabe A.2--")
    print (tzr_zeuge "Urknallexplosion" "knall")
    print (tzr_zeuge "Urknallexplosion" "Knall")
    print (tzr_zeuge "Urknallexplosionsknall" "knall")
    print (tzr_zeuge "Urknall" "")
    putStrLn ""

    print ("--Aufgabe A.3--")
    print (tzr_zeugen "Urknallexplosion" "knall")
    print (tzr_zeugen "Urknallexplosion" "Knall")
    print (tzr_zeugen "Urknallexplosionsknall" "knall")
    print (tzr_zeugen "Ur" "")
    putStrLn ""

    print ("--Aufgabe A.4--")
    print (wieOft "Urknallexplosion" "knall")
    print (wieOft "Urknallexplosion" "Knall")
    print (wieOft "Urknallexplosionsknall" "knall")
    print (wieOft "Ur" "")
    putStrLn ""
