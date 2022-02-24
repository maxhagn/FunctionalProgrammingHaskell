data Figure = Koenig | Dame | Bauer | Turm | Laeufer | Springer | Leer deriving (Show)
data Row = R [Figure]
data Field = F [Row]


instance Show Row where
 show (R b) = (show b ) ++ "\n"

instance Show Field where
 show (F b) = show b


main = do
  print "Chess"
  print ( show ( F [R [Koenig,Bauer],R [Koenig,Koenig]] ))