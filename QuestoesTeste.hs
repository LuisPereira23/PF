{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE FlexibleContexts #-}
import Data.List
import Data.Char



-- 1) Constrói a lista dos números inteiros compreendidos entre dois limites
myEnumFromTo :: Int -> Int -> [Int]
myEnumFromTo x y  | x == y = x:[]
                  | x > y = []
                  | otherwise = x : (myEnumFromTo (x+1) y)


-- 2) Constrói a lista dos números inteiros compreendidos entre dois limites e espaçados de um valor constante
myEnumFromThenTo :: Int -> Int -> Int -> [Int]
myEnumFromThenTo a b c =
	if a > c || a > b || b > c
	then []
	else a:(myEnumFromThenTo b (b+(b-a)) c )


-- 3) Concatena duas listas
(+++) :: [a] -> [a] -> [a]
(+++) [] l = l
(+++) l [] = l
(+++) (h:t) l = h : ((+++) t l)


-- 4) Dada uma lista e um inteiro, calcula o elemento da lista que se encontra nessa posição (assume-se que o primeiro elemento se encontra na posição 0)
(!!!) :: [a] -> Int -> a
(!!!) [] _ = undefined
(!!!) (h:t) 0 = h
(!!!) (h:t) n = (!!!) t (n-1)


-- 5) Dada uma lista calcula uma lista com os elementos dessa lista pela ordem inversa
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (h:t) = (myReverse t) ++ [h]


-- 6) Dado um inteiro n e uma lista l calcula a lista com os (no máximo) n primeiros elementos de l
myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake 0 l = []
myTake n (h:t) = h : (myTake (n-1) t)


-- 7) Dado um inteiro n e uma lista l calcula a lista sem os (no máximo) n primeiros elementos de l
myDrop :: Int -> [a] -> [a]
myDrop n [] = []
myDrop 0 l = l
myDrop n (h:t) = myDrop (n-1) t


-- 8) Constói uma lista de pares a partir de duas listas
myZip :: [a] -> [b] -> [(a,b)]
myZip l [] = []
myZip [] l = []
myZip (x:xs) (y:ys) = (x,y) : (myZip xs ys)


-- 9) Testa se um elemento ocorre numa lista
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (h:t) = 
	if x == h
	then True 
	else myElem x t


-- 10) Dado um inteiro n e um elemento x constrói uma lista com n elementos, todos iguais a x
myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n x = x : (myReplicate (n-1) x)


-- 11) Dado um elemento e uma lista, constrói uma lista em que o elemento fornecido é intercalado entre os elementos da lista fornecida
myIntersperse :: a -> [a] -> [a]
myIntersperse _ [] = []
myIntersperse _ [y] = [y]
myIntersperse x (h:t) = [h] ++ x : (myIntersperse x t) 


-- 12) Agrupa elementos iguais e consecutivos de uma lista
myGroup :: Eq a => [a] -> [[a]]
myGroup [] = []
myGroup (h:t) = (auxGroup (h:t)) : (myGroup t)

auxGroup :: Eq a => [a] -> [a]
auxGroup [] = []
auxGroup (h:t) = 
	if (h == (head t)) 
	then h:(auxGroup t) 
	else [h]


-- 13) Concatena as listas de uma lista
myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (h:t) = h ++ (myConcat t)


-- 14) Calcula a lista dos prefixos de uma lista
myInits :: Eq a => [a] -> [[a]]
myInits [] = []
myInits l = auxInits 0 l

auxInits :: Eq a => Int -> [a] -> [[a]]
auxInits _ [] = []
auxInits n l =
	if n < length l
	then (take n l) : (auxInits (n+1) l)
	else [l] 


-- 15) Calcula a lista dos sufixos de uma lista
myTails :: Eq a => [a] -> [[a]]
myTails [] = []
myTails l = auxTails 0 l

auxTails :: Eq a => Int -> [a] -> [[a]]
auxTails _ [] = []
auxTails n l =
	if n <= length l
	then (drop n l) : (auxTails (n+1) l)
	else []


-- 16) Testa se uma lista é prefixo de outra
myIsPrefixOf :: Eq a => [a] -> [a] -> Bool
myIsPrefixOf [] _ = True
myIsPrefixOf _ [] = False
myIsPrefixOf (x:xs) (y:ys) = 
	if x == y
	then myIsPrefixOf xs ys
	else False


-- 17) Testa se uma lista é sufixo de outra
myIsSuffixOf :: Eq a => [a] -> [a] -> Bool
myIsSuffixOf [] _ = False
myIsSuffixOf _ [] = True
myIsSuffixOf l1 l2 = myIsPrefixOf (myReverse l1) (myReverse l2)


-- 18) Testa se os elementos de uma lista ocorrem noutra pela mesma ordem relativa
myIsSubsequenceOf :: Eq a => [a] -> [a] -> Bool
myIsSubsequenceOf [] _ = True
myIsSubsequenceOf _ [] = False
myIsSubsequenceOf (x:xs) (y:ys) = 
	if x == y
	then myIsSubsequenceOf xs ys
	else myIsSubsequenceOf (x:xs) ys


-- 19) Calcula a lista de posições em que um dado elemento ocorre numa lista
myElemIndices :: Eq a => a -> [a] -> [Int]
myElemIndices _ [] = []
myElemIndices x (h:t) = auxElemIndices 0 x (h:t)

auxElemIndices :: Eq a => Int -> a -> [a] -> [Int]
auxElemIndices _ _ [] = []
auxElemIndices n x (h:t) = 
	if n < length (h:t)
	then 
		if x == h
		then n : (auxElemIndices (n+1) x t)
		else auxElemIndices (n+1) x t
	else []


-- 20) Calcula uma lista com os mesmos elementos da recebida, sem repetições
myNub :: Eq a => [a] -> [a]
myNub [] = []
myNub (h:t) = h : myNub (myRemove h t)

myRemove :: Eq a => a -> [a] -> [a]
myRemove _ [] = []
myRemove x (h:t) =
	if x == h
	then myRemove x t
	else h : (myRemove x t)


-- 21) Retorna a lista resultante de remover (a primeira ocorrência de) um dado elemento de uma lista
myDelete :: Eq a => a -> [a] -> [a]
myDelete _ [] = []
myDelete x (h:t) =
	if x == h
	then t
	else h : (myDelete x t)


-- 22) Retorna a lista resultante de remover (as primeiras ocorrências) dos elementos da segunda lista da primeira
(\\\) :: Eq a => [a] -> [a] -> [a]
(\\\) [] _ = []
(\\\) l [] = l
(\\\) (x:xs) (y:ys) = 
	if x == y
	then (\\\) xs ys
	else x : ((\\\) xs (y:ys))


-- 23) Retorna a lista resultante de acrescentar à primeira lista os elementos da segunda que não ocorrem na primeira
myUnion :: Eq a => [a] -> [a] -> [a]
myUnion l [] = l
myUnion [] l = l
myUnion l (h:t) = 
	if myElem h l
	then myUnion l t
	else myUnion (l ++ [h])  t


-- 24) Retorna a lista resultante de remover da primeira lista os elementos que não pertencem à segunda
myIntersect :: Eq a => [a] -> [a] -> [a]
myIntersect [] _ = []
myIntersect l [] = l
myIntersect (h:t) l =
	if myElem h l
	then  h : (myIntersect t l)
	else myIntersect t l


-- 25) Dado um elemento e uma lista ordenada retorna a lista resultante de inserir ordenadamente esse elemento na lista
myInsert :: Ord a => a -> [a] -> [a]
myInsert x [] = [x]
myInsert x (h:t) = 
	if x <= h
	then x : (h:t)
	else h : (myInsert x t)


-- 26) Junta todas as strings da lista numa só, separando-as por um espaço
myUnwords :: [String] -> String
myUnwords [] = ""
myUnwords [x] = x
myUnwords (h:t) = h ++ " " ++ (myUnwords t)


-- 27) Junta todas as strings da lista numa só, separando-as pelo caracter '\n'
myUnlines :: [String] -> String
myUnlines [] = ""
myUnlines [x] = x 
myUnlines (h:t) = h ++ "\n" ++ (myUnlines t)


-- 28) Dada uma lista não vazia, retorna a posição onde se encontra o maior elemento da lista. As posições da lista começam em 0, i.e., a função deverá retornar 0 se o primeiro elemento da lista for o maior
pMaior :: Ord a => [a] -> Int
pMaior [x] = 0
pMaior (h:t) = auxPMaior (h:t) h 0

auxPMaior :: Ord a => [a] -> a -> Int -> Int
auxPMaior [] _ i = i
auxPMaior (h:t) x i = 
	if h <= x
	then auxPMaior t x i
	else auxPMaior t h (i+1)


-- 29) Testa se uma lista tem elementos repetidos
temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (h:t) = 
	if myElem h t
	then True
	else temRepetidos t

 
-- 30) Determina a lista dos algarismos de uma dada lista de caracteres
algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (h:t) =
	if h == '0' || h =='1' || h =='2' || h =='3' || h =='4' || h =='5' || h =='6' || h =='7' || h =='8' || h =='9' 
	then h : (algarismos t)
	else algarismos t


-- 31) Determina os elementos de uma lista que ocorrem em posições ímpares. Considere que o primeiro elemento da lista ocorre na posiçãao 0 e por isso par
posImpares :: [a] -> [a]
posImpares [] = []
posImpares l = auxPosImpares l 0

auxPosImpares :: [a] -> Int -> [a]
auxPosImpares [] _ = []
auxPosImpares (h:t) x = 
	if (mod x 2)  == 1
	then h : (auxPosImpares t (x+1))
	else auxPosImpares t (x+1)


-- 32) Determina os elementos de uma lista que ocorrem em posições pares. Considere que o primeiro elemento da lista ocorre na posição 0 e por isso par
posPares :: [a] -> [a]
posPares [] = []
posPares l = auxPosPares l 0

auxPosPares :: [a] -> Int -> [a]
auxPosPares [] _ = []
auxPosPares (h:t) x = 
	if (mod x 2) == 0
	then h : (auxPosPares t (x+1))
	else auxPosPares t (x+1)


-- 33) Testa se uma lista está ordenada por ordem crescente
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (h:(t:ts)) = 
	if h > t
	then False
	else isSorted (t:ts)


-- 34) Calcula o resultado de ordenar uma lista. Assuma, se precisar, que existe defnida a função insert :: Ord a => a -> [a] -> [a] que dado um elemento e uma lista ordenada retorna a lista resultante de inserir ordenadamente esse elemento na lista
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert h (iSort t)


-- 35) Dadas duas strings, retorna True se e só se a primeira for menor do que a segunda, segundo a ordem lexicográfca (i.e., do dicionário)
menor :: String -> String -> Bool 
menor _ [] = False
menor [] _ = True
menor (x:xs) (y:ys) 
	| ord(x) < ord(y) = True
	| ord(x) > ord(y) = False
	| otherwise = menor xs ys


-- 36) Testa se um elemento pertence a um multi-conjunto
elemMSet :: Eq a => a -> [(a , Int)] -> Bool
elemMSet _ [] = False
elemMSet x ((a,b) : t) =
	if x == a 
	then True
	else elemMSet x t


-- 37) Calcula o tamanho de um multiconjunto
lengthMSet :: [(a , Int)] -> Int
lengthMSet [] = 0
lengthMSet ((a,b) : t) = b + (lengthMSet t)


-- 38) Converte um multi-conjuto na lista dos seus elementos
converteMSet :: [(a , Int)] -> [a]
converteMSet [] = []
converteMSet ((a,0) : t) = converteMSet t
converteMSet ((a,b) : t) = [a] ++ (converteMSet ((a , b-1) : t))


-- 39) Acrescenta um elemento a um multi-conjunto
insereMSet :: Eq a => a -> [(a , Int)] -> [(a , Int)] 
insereMSet x ((a,b) : t) = 
	if elemMSet x ((a,b) : t)
	then incMSet x ((a,b) : t)
	else ((a,b) : t) ++ [(x , 1)]

incMSet :: Eq a => a -> [(a , Int)] -> [(a , Int)]
incMSet x ((a,b) : t) = 
	if x == a 
	then ((a , (b+1)) : t)
	else (a,b) : (incMSet x t)


-- 40) Remove um elemento a um multi-conjunto. Se o elemento não existir, deve ser retornado o multi-conjunto recebido
removeMSet :: Eq a => a -> [(a , Int)] -> [(a , Int)]
removeMSet _ [] = []
removeMSet x ((a,b) : t) = 
	if (x == a)
	then 
		if b == 1 
		then t
		else (a , (b-1)) : t
	else (a,b) : (removeMSet x t)


-- 41) dada uma lista ordenada por ordem crescente, calcula o multi-conjunto dos seus elementos
constroiMSet :: Ord a => [a] -> [(a , Int)]
constroiMSet [] = []
constroiMSet [x] = [(x , 1)]
constroiMSet (h:t) = insereMSet h (constroiMSet t)


-- 42) Divide uma lista de Eithers em duas listas
partitionEithers :: [Either a b] -> ([a] , [b])
partitionEithers l = ((auxLeft l) , (auxRight l))

auxLeft :: [Either a b] -> [a]
auxLeft [] = []
auxLeft ((Left h) : t) = h : (auxLeft t)
auxLeft ((Right h) : t) = auxLeft t

auxRight :: [Either a b] -> [b]
auxRight [] = []
auxRight ((Left h) : t) = auxRight t
auxRight ((Right h) : t) = h : (auxRight t)


-- 43) Colecciona os elementos do tipo a de uma lista
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes [Nothing] = []
catMaybes ((Just h) : t) = h : (catMaybes t)
catMaybes ((Nothing) : t) = catMaybes t


-- 44) Dada uma posição inicial (coordenadas) e uma lista de movimentos, calcula a posição final do robot depois de efectuar essa sequência de movimentos
data Movimento = Norte | Sul | Este | Oeste
    deriving Show

posicao :: Eq Movimento => (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (h:t) 
	| h == Norte = posicao (x , y+1) t
	| h == Sul = posicao (x , y-1) t
	| h == Este = posicao (x+1 , y) t
	| otherwise = posicao (x-1 , y) t


 -- 45) Dadas as posições inicial e final (coordenadas) do robot, produz uma lista de movimentos suficientes para que o robot passe de uma posição para a outra
caminho :: Eq Movimento => (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (a,b) 
	| (x == a && y == b) = [] 
	| x < a = Este : (caminho (x+1 , y) (a,b))
	| x > a = Oeste : ((caminho (x-1 , y) (a,b)))
	| y < b = Norte : (caminho (x , y+1) (a,b))
	| otherwise = Sul : (caminho (x , y-1) (a,b))


-- 46) Testa se uma lista de movimentos só é composta por movimentos verticais (Norte ou Sul)
vertical :: Eq Movimento => [Movimento] -> Bool
vertical [] = False
vertical (h:t) = 
	if (h == Norte )|| (h == Sul)
	then vertical t
	else False


-- 47) Dada uma lista não vazia de posições, determina a que está mais perto da origem (note que as coordenadas de cada ponto são números inteiros)
data Posicao = Pos Int Int
    deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral (h:t) = auxGuardaPosicao (h:t) 0 h

auxGuardaPosicao :: [Posicao] -> Int -> Posicao -> Posicao
auxGuardaPosicao [] _ i = i
auxGuardaPosicao ((Pos a b) : t) x i =
	if sqrt ((a^2) + (b^2)) < x
	then auxGuardaPosicao t (sqrt (a^2 + b^2)) (Pos a b)
	else auxGuardaPosicao t x i






























