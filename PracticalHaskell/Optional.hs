data Optional a = Nada | Only a deriving(Eq,Show)

instance Monoid a => Monoid (Optional a) where
	mempty = Nada
	mappend (Only a) (Only b) = Only (mappend a b)
	mappend Nada (Only a) = (Only a)
	mappend (Only a) Nada = (Only a)
	mappend Nada Nada = Nada

newtype First' a = First'{ getFirst':: Optional a } deriving(Eq,Show)

instance Monoid (First' a)where
    mempty=First' {getFirst' = Nada}
    mappend (First' Only a) _= First'{ getFirst':: Only a }
    mappend First' Nada a = mappend a