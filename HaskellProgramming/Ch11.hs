data Price = Price Integer deriving (Eq,Show)

data Manufacturer=
                Mini
                | Mazda
                | Tata
                 deriving(Eq,Show)
data Airline=
            PapuAir
            | CatapultsR'Us
            | TakeYourChancesUnited
            deriving(Eq,Show)

data Vehicle= 
	        Car Manufacturer Price 
            | Plane Airline deriving(Eq,Show)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True 
isCar _ = False

areCars :: [Vehicle] -> [Bool]
areCars [] = []
areCars (x: xs) =  isCar x : areCars xs 