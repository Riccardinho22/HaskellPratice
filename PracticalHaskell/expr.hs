{-# LANGUAGE DeriveFunctor #-}
import Control.Applicative -- Otherwise you can't do the Applicative instance.

data Expr a = Var a | Add (Expr a) (Expr a) deriving (Show,Functor)

instance Applicative Expr where  
    pure = Var  
    (Var f) <*> something = fmap f something  
    (Add f g) <*> something = Add (f <*> something) (g<*> something)
instance Monad Expr where
 	return x = Var x
 	(Var a) >>= f = f a
 	(Add x y) >>= f = Add (x>>=f) (y>>=f)

replace :: Eq a => [(a,b)] -> Expr a -> Expr (Maybe b)
replace x y = y >>= \k-> return(lookup k x)

convert :: Expr (Maybe a)-> Maybe (Expr a)
convert (Var a) = do{x<-a;return(Var x)}
convert (Add x y) = do{ a <- convert x;
						b <- convert y;
						return (Add a b)}