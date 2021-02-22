import Data.List

--1
enumfromto :: Int->Int->[Int]
enumfromto a b = if a<=b then a: enumfromto (a+1) b else []
--2
enumfromthento :: Int->Int->Int->[Int]
enumfromthento a b c = if a<=c then a: enumfromthento (a+(b-1)) b c else []
--3
(+++) :: [a] -> [a] -> [a]
(+++) [] l = l
(+++) l [] = l
(+++) (h:t) l = h: (+++) t l
--4
(!!!) :: [a]->Int-> a
(!!!) (h:t) a = if a ==0 then h else (!!!) t (a-1)
--5
myreverse :: [a] -> [a]
myreverse [] = []
myreverse (h:t) = myreverse t ++ [h]
--6
mytake :: Int -> [a] -> [a]
mytake n [] = []
mytake 0 l = []
mytake n (h:t) = [h] ++ mytake (n-1) t 
--7
mydrop :: Int -> [a] -> [a]
mydrop n [] = []
mydrop 0 l = l
mydrop n (h:t) = mydrop (n-1) t
--8
myzip :: [a] -> [b] -> [(a,b)]
myzip [] l = []
myzip l [] = []
myzip (x:xs) (y:ys) = [(x,y)] ++ myzip xs ys
--9
myelem ::  Eq a => a -> [a] ->Bool
myelem a [] = False
myelem a (h:t) = if a == h then True else myelem a t
--10
myreplicate ::  Int -> a ->[a]
myreplicate 0 a = []
myreplicate n a = [a]++ myreplicate (n-1) a
--11
myintersperse ::  a -> [a] ->[a]
myintersperse a [] = []
myintersperse a (h:t) = [h,a] ++ myintersperse a t
--12
mygroup:: Eq a => [a]-> [[a]]
mygroup [] = []
mygroup (x:t) = [l1] ++ mygroup l2 where (l1,l2) = span ( ==x) (x:t)
--13
myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (h:t) = h ++ myconcat t
--14
myinits :: [a]->[[a]]
myinits a = aux2 a [] where
                        aux2 [] l = [l]
                        aux2 (h:t) l = l : aux2 t ( l ++[h])
--15
mytails ::[a]->[[a]]
mytails (h:t) = (h:t): mytails t
mytails []=[[]]
--16
myIsPrefixOf :: Eq a => [a] -> [a] -> Bool
myIsPrefixOf [] _ = True
myIsPrefixOf _ [] = False
myIsPrefixOf (x:xs) (y:ys) = if x == y then myIsPrefixOf xs ys else False
--17
myisSuffixOf ::  Eq a => [a]-> [a] -> Bool
myisSuffixOf [] _ = True
myisSuffixOf _ [] = False
myisSuffixOf l1 l2 = isPrefixOf (reverse l1) (reverse l2)
--18
subsequence :: Eq a => [a] ->[a] ->Bool
subsequence _ [] = False
subsequence [] _ = True
subsequence (x:xs) (y:ys) = if x==y 
                            then subsequence xs ys 
                            else subsequence (x:xs) ys
--19
elemind :: Eq a => a ->[a] -> [Int]
elemind a l = elemindaux 0 a l
elemindaux:: Eq a => Int -> a -> [a]->[Int]
elemindaux _ _ [] =[]
elemindaux pos ele (h:t) = if ele == h then pos: elemindaux (pos+1) ele t 
                                       else elemindaux (pos+1) ele t 
--20
nb::Eq a => [a] -> [a]
nb l = nbaux l [] where
       nbaux [] l2 = l2
       nbaux (x:xs) l2 = if elem x l2 then nbaux xs l2 else  nbaux xs (l2 ++ [x])        
--21
mydelete ::  Eq a => a -> [a]-> [a]   
mydelete _ [] = []
mydelete a l = deleteaux a l [] 
deleteaux :: Eq a => a -> [a]->[a]-> [a]   
deleteaux n (h:t) l2 = if n == h then l2 ++ t else deleteaux n t (h:l2)
--22
(///)::Eq a => [a] -> [a]-> [a]  
(///) [] _ = [] 
(///) l [] = l
(///) l (y:ys) = (///) (delete y l) ys 
--23
myunion ::  Eq a => [a] -> [a]-> [a]
myunion l [] = l
myunion [] _ = []
myunion l (x:xs) = if elem x l then myunion l xs else myunion (l++[x]) xs
--24
myintersect ::  Eq a => [a] ->[a] -> [a]
myintersect [] l = []
myintersect l [] = l
myintersect (x:xs) l = if elem x l then x:myintersect xs l else myintersect xs l
--25
myinsert ::  Ord a => a -> [a]-> [a]
myinsert a [] = [a]
myinsert a (x:xs) = if a > x then x: myinsert a xs else a:(x:xs)
--26
myunwords ::  [String] -> String
myunwords [] = []
myunwords (h:t)= h ++ ""++ myunwords t
--27
myunlines ::  [String] -> String
myunlines []=[]
myunlines (h:t) = h++"/n"++myunlines t
--28
pmaior:: Ord a => [a] -> Int
pmaior (h:t) = pmaiorx t h 0 where
               pmaiorx [] a pos = pos  
               pmaiorx (h:t) a pos = if h>a then pmaiorx t h (pos+1) 
                                                 else pmaiorx t a pos
--29
temRepetidos :: Eq a => [a] -> Bool
temRepetidos l = temRepetidosAux l [] where
                 temRepetidosAux [] l = False
                 temRepetidosAux (h:t) l = if elem h l then True else temRepetidosAux t ([h]++l)
--30
algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (h:t) = if h<='9'&&h>='0' then h: algarismos t else algarismos t
--31
posImpares :: [a] -> [a]
posImpares l = posImparesAux l 0 where
	           posImparesAux [] _ = []
	           posImparesAux (h:t) n = if  odd n then h:posImparesAux t (n+1) else posImparesAux t (n+1)
--32
posPares :: [a] -> [a]
posPares l = posParesAux l 0 where
             posParesAux [] _ = []
             posParesAux (h:t) n = if even n then h:posParesAux t (n+1) else posParesAux t (n+1)	
--33             
isorted::Ord a => [a] -> Bool 
isorted []= True
isorted [a]= True
isorted (a:b:c)= if a<b then isorted (b:c) else False
--34
isort ::Ord a => [a] -> [a]
isort l = sorta l [] where
          sorta [] l = l
          sorta (h:t) l = sorta t (insert h l)    
--35
menor::String -> String -> Bool
menor [] l = True
menor (x:xs) (y:ys) | x<y = True
                    | x>y = False
                    | x==y = menor xs ys   
--36
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet a ((x,y):t) = if a == x then True else elemMSet a t 
--37
lengthMSet ::  [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((x,y):t) = y + lengthMSet t
--38
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((x,y):t) = if y > 0 then x:converteMSet ((x,(y-1)):t) else converteMSet t
--39
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = [(a,1)]
insereMSet a ((x,y):t)= if a == x then ((x,(y+1)):t) else (x,y): insereMSet a t           
--40
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet a ((x,y):t) = if a == x then if y == 1 then t else ((x,(y-1)):t) else [(x,y)]++removeMSet a t
--41
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = constroiMSetAux h t 1 where
	                 constroiMSetAux a [] i = [(a,i)]
	                 constroiMSetAux a (h:t) i = if a == h then constroiMSetAux a t (i+1) else (a,i): constroiMSetAux h t 1
--42	                 
mypartitionEithers :: [Either a b] -> ([a],[b])
mypartitionEithers l = (left l, right l)
                    where
                      left  (Left a :t)   = a : left t
                      left  (Right b :t)  = left t
                      left _  = []
                      right (Left a :t)   = right t
                      right (Right a :t)  = a : right t
                      right _ = []

--------------------------------------------------------------------------------
-- Questão 43 --
--------------------------------------------------------------------------------

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just a : xs)  = a : catMaybes xs
catMaybes (Nothing : xs) = catMaybes xs

--------------------------------------------------------------------------------
-- Questão 44 --
--------------------------------------------------------------------------------

data Movimento = Norte | Sul | Este | Oeste
               deriving Show

posicao :: (Int, Int) -> [Movimento] -> (Int, Int)
posicao (x,y) []        = (x,y)
posicao (x,y) (Norte:r) = posicao (x,y+1) r
posicao (x,y) (Sul:r)   = posicao (x,y-1) r
posicao (x,y) (Este:r)  = posicao (x+1,y) r
posicao (x,y) (Oeste:r) = posicao (x-1,y) r


--------------------------------------------------------------------------------
-- Questão 45 --
--------------------------------------------------------------------------------

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x1,y1) (x2,y2) | x1==x2 && y1==y2 = []
                        | x1<x2            = [Este] ++ caminho (x1+1,y1) (x2,y2)
                        | x1>x2            = [Oeste] ++ caminho (x1,y1) (x2+1,y2)
                        | y1<y2            = [Norte] ++ caminho (x1,y1+1) (x2,y2)
                        | y1>y2            = [Sul] ++ caminho (x1,y1) (x2,y2+1)

--------------------------------------------------------------------------------
-- Questão 46 --
--------------------------------------------------------------------------------

vertical :: [Movimento] -> Bool
vertical []         = True
vertical (Norte:xs) = vertical xs
vertical (Sul:xs)   = vertical xs
vertical (Este:xs)  = False
vertical (Oeste:xs) = False

--------------------------------------------------------------------------------
-- Questão 47 --
--------------------------------------------------------------------------------

data Posicao = Pos Int Int
               deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral [x] = x
maisCentral (x:y:t) | aux x <= aux y = maisCentral (x:t)
                    | aux x >  aux y = maisCentral (y:t)
                  where
                    aux (Pos k m) = sqrt(fromIntegral(k^2+m^2))


--------------------------------------------------------------------------------
-- Questão 48 --
--------------------------------------------------------------------------------

vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos _ [] = []
vizinhos (Pos x y) ((Pos z w):t) = if (y == w && x == (z+1)) || (y == w && x == (z-1)) || (x == z && y == (w+1)) || (x == z && y == (w-1))
                                   then (Pos z w):vizinhos (Pos x y) t
                                   else vizinhos (Pos x y) t

--------------------------------------------------------------------------------
-- Questão 49 --
--------------------------------------------------------------------------------

mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [x] = True
mesmaOrdenada ((Pos x y):(Pos z w):t) = if y==w
                                        then mesmaOrdenada ((Pos z w):t)
                                        else False

--------------------------------------------------------------------------------
-- Questão 50 --
--------------------------------------------------------------------------------

data Semaforo = Verde | Amarelo | Vermelho
                deriving Show

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK l = (aux l)<=1
              where
                aux [Vermelho] = 0
                aux [Verde] = 1
                aux [Amarelo] = 1
                aux (Vermelho:resto) = aux resto
                aux (Verde:resto) = 1+aux resto
                aux (Amarelo:resto) = 1+aux resto

--7
mydrop2 :: Int -> [a] -> [a]
mydrop2 n [] = []
mydrop2 0 l = l
mydrop2 n (h:t) = if n>0 then mydrop2 (n-1) t else (h:t)