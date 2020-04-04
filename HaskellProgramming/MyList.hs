data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Monoid (List a) where
	mempty = Nil
	mappend Nil xs = xs
	mappend xs Nil = xs
	mappend xs ys = Cons xs ys 
instance Functor List where
	fmap _ Nil = Nil
	fmap f (Cons x xs) = Cons (f x) (fmap f xs)
instance Applicative List where
	pure x = Cons x Nil
	(<*>) Nil _ = Nil
	Cons f fs <*> xs = mapped (fmap f xs) ((<*>) fs xs)