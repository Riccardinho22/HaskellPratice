twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
	x<-xs
	if even x
		then [x*x,x*x]
		else []
f:: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integer ->Maybe Integer
g i = 
	if even i
	then Just (i+1)
	else Nothing

h :: Integer -> Maybe String
h i = Just ("ciao"++show i)

doSomenthing n = 
	do
	a<-f n
	b<- g a
	c<- h b
	pure (a,b,c)