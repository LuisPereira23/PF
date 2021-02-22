import Data.List
--1
enumfromto :: Int->Int->[Int]
enumfromto a b = if b>=a then a: enumfromto (a+1) b else []

--2
enumfromthento :: Int->Int->Int->[Int]
enumfromthento a b c = if a<=c then a: enumfromthento b ( b+b-a) c else []

--3
(+++) :: [a]->[a]->[a]
(+++) [] l = l
(+++) (h:t) l = h: (+++) t l

--4
lst:: [a]->a
lst [h]=h
lst (h:t)= lst t

--5
innit:: [a]->[a]
innit [] =[]
innit [h] = []
innit (h:t) = h: innit t

--6
(!!!) :: [a]->Int-> a
(!!!) (h:t) a = if a ==0 then h else (!!!) t (a-1)

--7
revers :: [a]->[a]
revers [] = []
revers l = aux1 l [] 

aux1:: [a]->[a]->[a]
aux1 [] l = l
aux1 (h:t) l = aux1 t (h:l)

--8
tk :: Int -> [a]->[a]
tk a [] =[]
tk 0 l = []
tk a (h:t)= h : tk (a-1) t

--9
dp :: Int -> [a]->[a]
dp _ [] = []
dp 0 l = l
dp a (h:t) = dp (a-1) t

--10
zp :: [a]-> [b]->[(a,b)]
zp [] _ = []
zp _ [] = []
zp (x:xs) (y:ys) = (x,y): (zp xs ys)

--11
ele::Eq a =>a->[a]->Bool
ele _ [] = False
ele a (b:t) = if a==b then True else ele a t

--12
repli :: Int->a-> [a]
repli 0 _ = []
repli a b = b:repli (a-1) b

--13
inter :: a->[a]->[a]
inter a [b] = [b]
inter a (h:t) = h:a: inter a t

--14
goup:: Eq a => [a]-> [[a]]
goup [] = []
goup (x:t) = l1 :goup l2 where (l1,l2) = span ( ==x) (x:t)

--15
conc :: [[a]] ->[a]
conc []=[]
conc (x:xs) = x ++ conc xs

--16
initi :: [a]->[[a]]
initi a = aux2 a [] where
                        aux2 [] l = [l]
                        aux2 (h:t) l = l : aux2 t ( l ++[h])

--17
tailss ::[a]->[[a]]
tailss (h:t) = (h:t): tailss t
tailss []=[[]]

--18
prefixof:: Eq a => [a] ->[a] ->Bool
prefixof [] l= True
prefixof l [] = False
prefixof (x:xs) (y:ys) = if x==y then prefixof xs ys else False

--19
sufixof::Eq a => [a] ->[a] ->Bool
sufixof l1 l2 = prefixof ( reverse l1) (reverse l2)

--20
subsequence :: Eq a => [a] ->[a] ->Bool
subsequence _ [] = False
subsequence [] _ = True
subsequence (x:xs) (y:ys) = if x==y 
                            then subsequence xs ys 
                            else subsequence (x:xs) ys

--21
elemind :: Eq a => a ->[a] -> [Int]
elemind a l = elemindaux 0 a l
elemindaux:: Eq a => Int -> a -> [a]->[Int]
elemindaux _ _ [] =[]
elemindaux pos ele (h:t) = if ele == h then pos: elemindaux (pos+1) ele t 
                                       else elemindaux (pos+1) ele t 

--22
nb::Eq a => [a] -> [a]
nb l = nbaux l [] where
       nbaux [] l2 = l2
       nbaux (x:xs) l2 = if elem x l2 then nbaux xs l2 else  nbaux xs (l2 ++ [x])

--23
del:: Eq a => a -> [a]-> [a]
del a (x:xs) = if a== x then del a xs else x: del a xs
del a [] = []

--24
(\\\):: Eq a => [a] -> [a] -> [a]
(\\\) l [] = l
(\\\) [] _ =[]
(\\\) (x:xs) (y:ys) = if x==y then (\\\) xs ys else x: (\\\) xs (y:ys)

--25
uni :: Eq a => [a] -> [a]-> [a]
uni [] l = l
uni l []= l
uni l (x:xs) = if elem x l then uni l xs else uni (l ++ [x]) xs

--26
interseta :: Eq a => [a] ->[a] -> [a]
interseta [] _=[]
interseta (x:xs) l = if elem x l then x: interseta xs l else interseta xs l

--27
inserta:: Ord a => a -> [a]-> [a]
inserta a [] = [a]
inserta a (h:t) = if a>h then h: inserta a t else a:(h:t)

--28
maximu ::Ord a => [a] ->a
maximu (h:t) =maximuaux h t where
              maximuaux a [] =  a
              maximuaux a (h:t) = if a>h then maximuaux a t else maximuaux h t

--29
minimu ::Ord a => [a] ->a
minimu (h:t) =manimuaux h t where
              manimuaux a [] =  a
              manimuaux a (h:t) = if a>h then manimuaux h t else manimuaux a t

--30
soma::Num a => [a] -> a
soma []=0
soma (h:t) = h +soma t

--31
produto::Num a => [a] -> a
produto []=0
produto [a]=a
produto (h:t) = h *produto t

--32
eque:: [Bool] -> Bool
eque [a]=a
eque (h:t)= h && eque t

--33
ouque:: [Bool] -> Bool
ouque [a]=a
ouque (h:t)= h || eque t

--34
uwords::[String] -> String
uwords []=[]
uwords [a]=a
uwords (h:t) = h ++" " ++ uwords t

--35
ulines::[String] -> String
ulines []=[]
ulines [a]=a
ulines (h:t) = h ++"\n" ++ ulines t

--36
pmaior:: Ord a => [a] -> Int
pmaior (h:t) = pmaiorx t h 0 1 where
               pmaiorx [] a numa num =numa  
               pmaiorx (h:t) a numa num = if h>a then pmaiorx t h num (num+1) 
                                                 else pmaiorx t a numa (num+1)

--37
temrepetidos:: Eq a => [a] -> Bool
temrepetidos l = temrepau l [] where
                 temrepau [] l =False
                 temrepau (h:t) l = if elem h l then True else temrepau t ([h] ++ l)

--38
alg::[Char] -> [Char]
alg []=[]
alg (h:t)= if h<='9'&&h>='0' then h :alg t else alg t

--39
posimpares::[a] -> [a]
posimpares []=[]
posimpares [a]=[]
posimpares [a,b]=[b]
posimpares (a:b:c)=b:posimpares c

--40
pospares::[a]->[a]
pospares[]=[]
pospares[a]=[a]
pospares[a,b]=[a]
pospares(a:b:c)=a:pospares c

--41
isorted::Ord a => [a] -> Bool 
isorted []= True
isorted [a]= True
isorted (a:b:c)= if a<b then isorted (b:c) else False

--42
isort ::Ord a => [a] -> [a]
isort l = sorta l [] where
          sorta [] l = l
          sorta (h:t) l = sorta t (insert h l)

--43
menor::String -> String -> Bool
menor [] l = True
menor (x:xs) (y:ys) | x<y = True
                    | x>y = False
                    | x==y = menor xs ys 

--44
elemSet :: Eq a => a -> [(a,Int)] -> Bool
elemSet _ [] = False
elemSet a ((x,y):t) = if a==x then True else elemSet a t

--45
lengthMSet ::[(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((x,y):t) = y + lengthMSet t

--46
converteMset ::[(a,Int)] -> [a]
converteMset []=[]
converteMset((x,y):t) = (aux3 x y) ++ converteMset t

aux3:: a->Int->[a]
aux3 a num = if num>0 then a:aux3 a (num-1) else []

--47
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = []
insereMSet a ((x,y):t)= if a==x then (x,(y+1)):insereMSet a t else (x,y):insereMSet a t

--48
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet a [] = []
removeMSet a ((x,y):t)= if a==x then if y==1 then removeMSet a t else (x,(y-1)):removeMSet a t else (x,y):removeMSet a t

--49
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = aux4 h t 1  where
                aux4 a [] i = [(a,i)]
                aux4 a (h:t) i  = if a==h then aux4 a t (i+1)
                                        else (a,i): aux4 h t 1 

--50
somapares::[Int] -> Int
somapares []=0
somapares (h:t) = if mod h 2 ==0 then h + somapares t else somapares t 
