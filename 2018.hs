import Data.List

--1
enumfromto :: Int -> Int ->[Int]
enumfromto x 0 = []
enumfromto x y = x: enumfromto (x+1) (y-1)

--2
(+++) :: [a] -> [a] -> [a]
(+++) [] l = l
(+++) (h:t) l = h: (+++) t l


--3
enumfromthento :: Int -> Int-> Int -> [Int]
enumfromthento a b c = if a <= c then a: enumfromthento (a+(b-1)) b c else []

--4
(!!!) :: [a] -> Int -> a
(!!!) (h:t) x = if x==0 then h else (!!!) t (x-1)

--5
revers :: [a] -> [a]
revers [] = []
revers l = aux1 l []

aux1 :: [a]->[a]->[a]
aux1 [] l = l
aux1 (h:t) l = aux1 t (h:l) 

--6
takes :: Int -> [a] -> [a]
takes _ [] =[]
takes 0 l = []
takes a (h:t) = h: takes (a-1) t


--7
drope :: Int -> [a] -> [a]
drope _ []=[]
drope 0 l = l
drope a (h:t) = drope (a-1) t

--8
zipe :: [a] -> [b] -> [(a,b)]
zipe [] l = []
zipe l [] = []
zipe (x:xs) (y:ys) = (x,y): (zipe xs ys)

--9
elems :: Eq a => a -> [a] ->Bool
elems a [] = False
elems a (h:t) = if a==h then True else elems a t

--10
replicates :: Int -> a ->[a]
replicates 0 _ = [] 
replicates a b = b: replicates (a-1) b