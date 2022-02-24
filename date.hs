data Month = January | February | March | April | Mai
            | June | July | August | September
            | October | November | December  deriving (Eq,Show)


convertDef :: Month -> String
convertDef January = "January"
convertDef February = "February"

convertGuard :: Month -> String
convertGuard a
  | a == January = "January"
  | a == February = "February"


main = do
  print (convertDef ( January ))
  print (convertDef ( February ))

  print (convertGuard ( January ))
  print (convertGuard ( February ))

  print ( show ( January ) )
  print ( show ( December ) )
  print ( show ( March ) )