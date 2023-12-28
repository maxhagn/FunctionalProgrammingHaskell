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
ist_tzr string sequence = isInfixOf sequence string

{- Knapp, aber gut nachvollziehbar geht ist_tzr folgendermassen vor:
   Parameter für ist_tzr sind eine Zeichenreihe und eine Teilzeichenreihe, wobei ein boolean Wert zurückgegeben wird.
   isInfixOf überprüft ob der erste String im zweiten String enthalten ist und liefert einen boolean Wert zurück.
-}

-- Aufgabe A.2

tzr_zeuge :: Zeichenreihe -> Teilzeichenreihe -> Zerlegungszeuge

substringIndex :: (Eq a) => [a] -> [a] -> Int
substringIndex searchphrase string = fromMaybe (-1) $ findIndex (isPrefixOf searchphrase) (tails string)

substring :: Int -> Int -> String -> String
substring start end text = take (end - start) (drop start text)

tzr_zeuge string sequence | ist_tzr string sequence = ((substring 0 (substringIndex sequence string) string),
                                    sequence,
                                    (substring ((substringIndex sequence string) + length sequence) (length string) string))
                 | otherwise = ("", sequence ++ sequence, "")

{- Knapp, aber gut nachvollziehbar geht tzr_zeuge folgendermassen vor: 
   Als Parameter wird eine Zeichenreihe und eine Teilzeichenreihe angegeben, wobei ein Zerlegungszeuge zurückgegeben wird.
   Mit Hilfe der ist_tzr Funktion wird überprüft ob die Teilzeichenreihe in der Zeichenreihe enthalten ist.
   Ist die Teilzeichenreihe nicht enthalten wird das Tripel ("", tt, "") ausgegeben.
   Ist die Teilzeichenreihe enthalten, werden drei substrings als Tripel zusammengefasst.
   Der erste Substring geht von 0 bis zum Index der Teilzeichenreihe.
   Der zweite Substring ist die gesuchte Teilzeichenreihe.
   Der dritte Substring geht vom Ende der Teilzeichenreihe bis zum Ende der Zeichenreihe.
-}

-- Aufgabe A.3

tzr_zeugen :: Zeichenreihe -> Teilzeichenreihe -> Zerlegungszeugen

tzr_zeugen string sequence = [ ((substring 0 (length string - length tmp) string),
                             sequence,
                             (substring (length string - length tmp + length sequence) (length string) string))
                             | tmp <- tails string, isPrefixOf sequence tmp ]

{- Knapp, aber gut nachvollziehbar geht tzr_zeugen folgendermassen vor: 
   tzr_zeugen nimmt die Parameter Zeichenreihe und Teilzeichenreihe entgegen und gibt Zerlegungszeugen zurück.
   Für jedes Vorkommen von Teilzeichenreihe in Zeichenreihe wird ein Zerlegungszeuge erstellt und einem Array hinzugefügt.
   Die Zerlegungszeugen werden wie in Aufgabe A.2 gebildet, wobei das Tripel ("", tt, "") nicht ausgegeben wird.
-}


-- Aufgabe A.4

wieOft :: Zeichenreihe -> Teilzeichenreihe -> Nat0

wieOft sequence string = sum [ 1 | tmp <- tails sequence, isPrefixOf string tmp ]

{- Knapp, aber gut nachvollziehbar geht wieOft folgendermassen vor:
   wieOft nimmt die Parameter Zeichenreihe und Teilzeichenreihe entgegen und gibt eine Natürliche Zahl beginnen bei 0 zurück.
   Für jedes Vorkommen von Teilzeichenreihe in Zeichenreihe wird ein Einser zu einem Array hinzugefügt.
   sum zählt die Zahlenwerte im Array, also die Vorkommen von Teilzeichenreihe.
-}

main = do
    print (ist_tzr "Urknallexplosion" "knall")
    print (ist_tzr "Urknallexplosion" "Knall")
    print (ist_tzr "Urknallexplosion" "")
    print (ist_tzr "" "Urknallexplosion")
    print (ist_tzr "Urknallexplosionsknall" "knall")
    putStrLn ""

    print (tzr_zeuge "Urknallexplosion" "knall")
    print (tzr_zeuge "Urknallexplosion" "Knall")
    print (tzr_zeuge "Urknallexplosionsknall" "knall")
    print (tzr_zeuge "Urknall" "")
    putStrLn ""

    print (tzr_zeugen "Urknallexplosion" "knall")
    print (tzr_zeugen "Urknallexplosion" "Knall")
    print (tzr_zeugen "Urknallexplosionsknall" "knall")
    print (tzr_zeugen "Ur" "")
    putStrLn ""

    print (wieOft "Urknallexplosion" "knall")
    print (wieOft "Urknallexplosion" "Knall")
    print (wieOft "Urknallexplosionsknall" "knall")
    print (wieOft "Ur" "")
    putStrLn ""