{-
Commentary Section
-}

type Nat0 = Integer
type Nat1 = Integer
data Jahreszeiten = Fruehling | Sommer | Herbst | Winter
type Vorname = [Char]
type Nachname = [Char]
data Geschlecht = Maennlich | Weiblich
data Person = P Vorname Nachname Geschlecht
data A = String | Integer

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

add :: Integer -> Integer -> Integer
add a b = a + b

sub :: Integer -> Integer -> Integer
sub a b = a - b

addGeneric :: Num a => a -> a -> a
addGeneric x y = x + y

-- Even Odd Implementation
isEven :: Integer -> Bool
isEven n
  | n == 0 = True
  | n > 0 = isOdd (n-1)
  | n < 0 = isOdd (n+1)

isOdd :: Integer -> Bool
isOdd n
  | n == 0 = False
  | n > 0 = isEven (n-1)
  | n < 0 = isEven (n+1)

-- Quicksort Implementation
quickSort :: [Integer] -> [Integer]
quickSort [] = []
quickSort (n:ns) = quickSort [m | m <- ns, m <= n]
                   ++ [n]
                   ++ quickSort [m | m <- ns, m > n]

-- Mergesort Implementation
mergeSort :: [Integer] -> [Integer]
mergeSort [] = []
mergeSort (n:ns) = quickSort [m | m <- ns, m <= n]
                  ++ [n]
                  ++ quickSort [m | m <- ns, m > n]


-- Diff Implementation
differenceLists :: [Char] -> [Char] -> Nat0
differenceLists a b | length a == length b = compareLists ( zip a b ) 0
                    | True = error "Not the same length"

compareLists :: [(Char, Char)] -> Nat0 -> Nat0
compareLists [] i = i
compareLists (x:xs) i = if fst x == snd x then compareLists xs i else compareLists xs (i+1)

-- Sum List Implementation
sumList :: [Integer] -> Integer
sumList []     = 0
sumList a = foldl (min) 1 a

pw :: [Integer] -> Integer -> Integer
pw [] i = 0
pw (x:xs) i = x * ( i ^ length xs ) + pw xs i

-- Pruefung
data Nat = Null | Succ Nat deriving (Eq,Ord,Show)

plus :: Nat -> Nat -> Nat
plus Null x = x
plus x Null = x
plus Null Null = Null
plus ( Succ x ) y = Succ ( plus x y )

minus :: Nat -> Nat -> Nat
minus Null x = x
minus x Null = x
minus Null Null = Null
minus x y
  | x < y = Null
  | x == y = Null
minus (Succ x) (Succ y) = minus x y



main = do
  print ( show ( fac 1 ) )
  print ( show ( fac 2 ) )
  print ( show ( fac 3 ) )
  print ( show ( fac 4 ) )
  print ( show ( fac 5 ) )
  print ( returnHelloWorld "Max" )
  putStrLn ( returnHelloWorld "Max" )

  print ( show ( add 5 10 ) )
  print ( show ( add 10 10 ) )

  print ( show ( addGeneric 5 10 ) )
  print ( show ( addGeneric 10 10 ) )

  print ( show ( sub 5 10 ) )
  print ( show ( sub 10 10 ) )
  print ( show ( sub 15 10 ) )

  print ( show ( isOdd 11 ) )
  print ( show ( isOdd 12 ) )

  print ( show ( isEven 11 ) )
  print ( show ( isEven 12 ) )

  print ( show (differenceLists ['a'..'e'] ['c'..'g'] ) )
  print ( show (differenceLists "hallo" "bello" ) )
  print ( show (differenceLists "abcde" "cdefg" ) )
  --print ( show (differenceLists "sippi" "spi" ) )


  print ( show (sumList [1..100] ) )

  print ( show ( pw [1..10] 10 ) )

  print ( show ( plus (Succ (Succ (Succ (Succ (Succ (Succ Null)))))) (Succ (Succ (Succ (Succ (Succ (Succ Null)))))) ) )
  print ( show ( minus (Succ (Succ (Succ (Succ (Succ (Succ Null)))))) (Succ (Succ (Succ (Succ (Succ (Succ Null)))))) ) )
  print ( show ( minus (Succ (Succ (Succ (Succ (Succ (Succ Null)))))) (Succ (Succ (Succ (Succ (Succ Null))))) ) )
  print ( show ( minus Null (Succ (Succ (Succ (Succ (Succ (Succ Null)))))) ) )
  print ( show ( minus (Succ (Succ (Succ (Succ (Succ (Succ Null)))))) Null ) )