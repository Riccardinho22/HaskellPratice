class Nameable n where
  name :: n -> String

data Person   = Person { firstName :: String, lastName :: String }
              deriving (Show, Eq, Ord)
data Client i = GovOrg  { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String
                        , person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving (Show, Eq, Ord)
              
instance Nameable (Client i) where
  name Individual { person = Person { firstName = f, lastName = n } }
         = f ++ " " ++ n
  name c = clientName c

useNameable:: Nameable a => a->String
useNameable a = "Using Namable"  ++ (name a) 