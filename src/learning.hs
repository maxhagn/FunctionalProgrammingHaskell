{-
Commentary Section
-}

data Jahreszeiten = Fruehling | Sommer | Herbst | Winter
type Vorname = [Char]
type Nachname = [Char]
data Geschlecht = Maennlich | Weiblich
data Person = P Vorname Nachname Geschlecht

func :: Integer -> Integer
func n = 1

fac :: Integer -> Integer
fac n
  | n == 0    = 1
  | n > 0     = n * fac(n - 1)
  | otherwise = error "PANIKMODUS"

-- Hello World Implementation
returnHelloWorld :: String -> String
returnHelloWorld n = "Hello World, " ++ n ++ " : ) !"

-- Quicksort Implementation
quickSort :: [Integer] -> [Integer]
quickSort [] = []
quickSort (n:ns) = quickSort [m | m <- ns, m <= n]
                   ++ [n]
                   ++ quickSort [m | m <- ns, m > n]

mergeSort :: [Integer] -> [Integer]
mergeSort [] = []
mergeSort (n:ns) = quickSort [m | m <- ns, m <= n]
                  ++ [n]
                  ++ quickSort [m | m <- ns, m > n]
                  

main = do
  print ( show ( fac 1 ) )
  print ( show ( fac 2 ) )
  print ( show ( fac 3 ) )
  print ( show ( fac 4 ) )
  print ( show ( fac 5 ) )
  print ( returnHelloWorld "Max" )
  putStrLn ( returnHelloWorld "Max" )