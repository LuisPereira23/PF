--1
enumde :: Int -> Int -> [Int]
enumde 0 0 = []
enumde x y | x==y = []
           | x<y = x : (enumde (x+1) y) 

--2
enumfromthento1 :: Int -> Int-> Int -> [Int]
enumfromthento1  0 y 0 = []
enumfromthento1  x y z | x==z = []
                       | x>z =[]
                       | x<z = x : (enumfromthento1 (x+y) y z) 

--3

(+-+) :: [a] -> [a] -> [a]
(+-+) y [] = y
(+-+) [] x = x
(+-+) (x:xs) ys = x : xs ++ ys

--4

last1 :: [a] -> a
last1 (h:t) = if length t == 0 then h else last t

--5

init1 :: [a] -> [a]
init1 [x] = []
init1 (h:t) = h : init1 t   

--6

(!-!) :: [a] -> Int -> a
(!-!) [x] 0 = x 
(!-!) (h:t) y = if y == 0 then h else (!-!) t (y-1) 

--7
reverse1 :: [a] -> [a] 
reverse1 [] = []
reverse1 (h:t) = reverse1 t ++ [h]

--8
take1 :: Int -> [a] -> [a]
take1 0 x = []
take1 x (h:t) = if length (h:t)< x then (h:t) else [h] ++ take1 (x-1) t 

--9

drop1 :: Int -> [a] -> [a] 
drop1 0 y = y
drop1 x (h:t) = if length (h:t)< x then [] else drop1 (x-1) t

--10

zip1 :: [a] -> [b] -> [(a,b)]
zip1 x [] = []
zip1 [] y = []
zip1 (xh:xt) (yh:yt) = [(xh,yh)] ++ zip1 xt yt

--11

elem1 :: Eq a => a -> [a] ->Bool
elem1 x [] = False
elem1 x (h:t)= if x==h then True else elem1 x t

--12
replicate1 :: Int -> a ->[a]
replicate1 0 x = []
replicate1 x y = [y] ++ (replicate1 (x-1) y)



--13
intersperse1 :: a -> [a] ->[a]
intersperse1 x [] = []
intersperse1 x (h:t) = [h]++[x]++ (intersperse1 x t)
-- 14)
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' (h:ts) = l1 : group' l2 where (l1,l2) = span ( == h) (h:ts)

--14
---------------------------------------------------------------------------------------------------------
group1 :: Eq a => [a] -> [[a]]
group1 []=[]
group1 [x]=[[x]]
group1 (x:y:ys)| x==y = let (h:t) = group1 (y:ys) in (x:h):t
               | x/=y = [x] : group1(y:ys)

--15

concat1 :: [[a]] -> [a] 
concat1 [] = []
concat1 ((h:t):yt)= [h] ++ (concat1(t:yt))

concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:ts) = h ++ concat' ts

--16
inits1 :: [a]-> [[a]]
inits1 l = aux [] l
          where aux:: [a] -> [a] -> [[a]]
                aux a [] = [a]
                aux a (h:t)= a : aux (a++[h]) t
----------------------------------------------------------------------------------------------------------
--17
tails1 :: [a] -> [[a]] 
tails1 l = reverse (inits1 l)

--18

isPrefixOf1 :: Eq a => [a]-> [a] -> Bool 
isPrefixOf1 x y = if (take (length x) y) == x then True else False  

--19

isSufixOf1 :: Eq a => [a]-> [a] -> Bool 
isSufixOf1 x y = if (drop1 (length y - length x) y) == x then True else False  

--20 

isSubsequenceOf1 :: Eq a =>[a] -> [a] -> Bool
isSubsequenceOf1 (h:t) y = if aux 0 (h:t) y < aux 0 t y then True else False
                         where aux :: Eq a =>Int -> [a] -> [a] -> Int
                               aux x (xh:xt) (yh:yt) = if xh == yh then x else aux (x+1) (xh:xt) (yt)

isSubsequentOf' :: Eq a => [a] -> [a] -> Bool
isSubsequentOf' [] _ = True  
isSubsequentOf' _ [] = False
isSubsequentOf' (x:xs) (y:ys) = if (x == y) then isSubsequentOf' xs ys else isSubsequentOf' (x:xs) ys                               

--21 

elemIndices1 :: Eq a => a ->[a] -> [Int]
elemIndices1  x [] = []
elemIndices1  x y = aux 0 x y 
                  where aux :: Eq a =>Int -> a -> [a] -> [Int]
                        aux c x [] = []
                        aux c x (h:t) | h == x = [c] ++ (aux (c+1) x t)
                                      | h /= x = (aux (c+1) x t)

--22

nub1 ::Eq a => [a] -> [a]
nub1 [] = []
nub1 (x:xs) = x: nub1(apaga x xs)
            where apaga :: Eq a => a -> [a] -> [a]
                  apaga x [] = []
                  apaga x (y:ys)|x ==y = apaga x ys
                                |x /= y = y:apaga x ys
                                      
--23

delete1 :: Eq a => a -> [a]-> [a]
delete1 x [] = []
delete1 x (h:t) | x==h = t
                | x/=h = [h]++ (delete1 x t) 

--24
(\-\) :: Eq a => [a] -> [a] -> [a]
(\-\) [] _ = []
(\-\) list [] = list
(\-\) list (x:xs) = (\-\) (delete1 x list) xs

--25

union1 :: Eq a => [a] -> [a]-> [a]
union1 x [] = x
union1 [] y = y
union1 x (yh:yt) = if elem1 yh x

 then union1 x yt else(union1 (x++[yh]) yt )  


--26
intersect1 :: Eq a => [a] ->[a] -> [a]
intersect1 x [] = []
intersect1 [] y = []
intersect1 (xh:xt) y = if elem1 xh y then [xh] ++ (intersect1 xt y) else  (intersect1 xt y)

--27

insert1 :: Ord a => a -> [a]-> [a]
insert1 x [] = [x]
insert1 x (h:t)|x<h = [x]++[h]++t 
               |x>h = [h]++(insert1 x t) 

--28

maximum1 :: Ord a => [a] ->a
maximum1 (xh:xt) = aux xh xt
               where aux ::Ord a => a -> [a]-> a
                     aux x []= x
                     aux x (yh:yt)|x>yh = aux x yt
                                  |x<yh = aux yh yt

--29
minimum1 :: Ord a => [a] ->a          
minimum1 (xh:xt) = aux xh xt
               where aux ::Ord a => a -> [a]-> a
                     aux x []= x
                     aux x (yh:yt)|x<yh = aux x yt
                                  |x>yh = aux yh yt
--30

sum1 :: Num a => [a] -> a
sum1 [] = 0
sum1 (h:t)= h + (sum1 t)

--31

product1 :: Num a => [a] ->a
product1 [] = 1
product1 (h:t) = h * (product1 t)

--32
and' :: [Bool] -> Bool
and' [] = True
and' (h:ts) = if (h) then and' ts else h

-- 33)
or' :: [Bool] -> Bool
or' [] = False
or' (h:ts) = if (h) then h else or' ts

-- 34)
unwords' :: [String] -> String
unwords' [] = []
unwords' [x] = x
unwords' (h:ts) = h ++ " " ++ unwords' ts

-- 35)
unlines' :: [String] -> String
unlines' [] = []
unlines' [x] = x  -- na definição original, este ponto de paragem não existe;
unlines' (h:ts) = h ++ "\n" ++ (unlines' ts)

-- 37)
temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (h:ts) = if (elem h ts) then True else temRepetidos ts
-- 38)
alg::[Char] -> [Char]
alg []=[]
alg (h:t)= if h<='9'&&h>='0' then h :alg t else alg t

--39
posImpares :: [a] -> [a]
posImpares [] = []
posImpares [x] = []
posImpares [x,y] = [y]
posImpares (h:m:ts) = m : posImpares ts

--40
posPares :: [a] -> [a]
posPares [] = []
posPares [x] = [x]
posPares [x,y] = [x]
posPares (h:m:ts) = h : posPares ts

-- 41)
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (h:ts) = if (h > (head ts)) then False else isSorted ts

-- 42)
iSort :: Ord a => [a] -> [a]
iSort list = iSortAux list [] where
  iSortAux [] new = new
  iSortAux (h:ts) new = iSortAux ts (insert1 h new)

--43
menor::String -> String -> Bool
menor [] l = True
menor (x:xs) (y:ys) | x<y = True
                    | x>y = False
                    | x==y = menor xs ys 

-- 44)
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet z ((x,y):ts) = if (z == x) then True else elemMSet z ts

-- 45)
lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((x,y):ts) = y + (lengthMSet ts)

-- 46)
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((x,y):ts) = if (y > 0) then x : (converteMSet ((x,(y - 1)):ts)) else converteMSet ts    

-- 47)
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet z [] = [(z,1)]
insereMSet z ((x,y):ts) = if (z == x) then (x,(y + 1)) : ts else (x,y) : (insereMSet z ts)

-- 48)
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet z ((x,y):ts) = if (z == x) then (if (y > 1) then (x,(y - 1)) :ts else ts) else (x,y) : (removeMSet z ts)

-- 49)
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet list = cMSAux (tail list) (head list) 1 [] where
  cMSAux [] agora qts new = new ++ [(agora, qts)]
  cMSAux (h:ts) agora qts new = if (h == agora) then cMSAux ts agora (qts + 1) new else cMSAux ts h 1 (new ++ [(agora, qts)])

-- 50)
somaPares :: [Int] -> Int
somaPares [] = 0
somaPares (h:ts) = if (mod h 2 == 0) then h + (somaPares ts) else somaPares ts


