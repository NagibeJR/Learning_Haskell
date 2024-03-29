import Data.Char

-- localizar se é diferente

diferente3 :: Int->Int->Int->Bool
diferente3 a b c = a/=b && b/=c && a/=c

--localizar se é igual

iguais3 :: Int -> Int -> Int -> Bool
iguais3 a b c = a == b && b == c

--localizar se dois numeros são iguas

iguais2de3 :: Int -> Int -> Int -> Bool
iguais2de3 a b c = a == b || b == c || a == c

-- criando a função do triangulo

formaTri :: Int->Int->Int->Bool
formaTri a b c = (a + b > c) && (a + c > b) && (b + c > a)

--verificar triangulo escaleno

escalenoTri :: Int -> Int -> Int -> Bool
escalenoTri a b c = (formaTri a b c) && (diferente3 a b c)

--verificar triangulo equilatero

equilateroTri :: Int -> Int -> Int -> Bool
equilateroTri a b c = (formaTri a b c) && (iguais3 a b c)

--verificar triangulo isoceles

isocelesTri :: Int -> Int -> Int -> Bool
isocelesTri a b c = (formaTri a b c) && (iguais2de3 a b c)

-- função recursiva da formula de fibonaicci
fibo :: Int -> Int
fibo 1 = 1
fibo 2 = 1
fibo n = fibo(n-1) + fibo(n-2)

--fução par ou impar

parimpar :: Int -> IO()
parimpar num
 |(mod num 2 == 0) = putStrLn "par"
 |(mod num 2 == 1) = putStrLn "impar"

--função para verificar se é multiplo de 2, 3 e 5

multi :: Int -> IO()
multi num
 |(mod num 2 == 0) && (mod num 3 == 0) && (mod num 5 == 0) = putStrLn "Multiplo de 2, 3 e 5"
 |(mod num 2 == 0) && (mod num 3 == 0) = putStrLn "Multiplo de 2 e 3"
 |(mod num 2 == 0) && (mod num 5 == 0) = putStrLn "Multiplo de 2 e 5"
 |(mod num 3 == 0) && (mod num 5 == 0) = putStrLn "Multiplo de 3 e 5"
 |(mod num 2 == 0) = putStrLn "Multiplo de 2"
 |(mod num 3 == 0) = putStrLn "Multiplo de 3"
 |(mod num 5 == 0) = putStrLn "Multiplo de 5"
 |otherwise = error "Nao e mulltiplo de 2 || 3 || 5" 

--Listas

--conta a quantidade de elementos no vetor

conta :: [Int] -> Int
conta [] = 0
conta(a:b) = 1 + conta b

--dobra os elementos na lista
dobra :: [Int] -> [Int]
dobra [] = []
dobra(a:b) = (2 * a : dobra b)

--primeiro indice da lista par

pares :: [Int] -> [Int]
pares [] = []
pares(a:b) 
 |(mod a 2 == 0) = a : pares b
 |(mod a 2 == 1) = pares b

--recebe lista e fica os N primeiros da lista

escolheN :: Int -> [Int] -> [Int]
escolheN _[] = []
escolheN n (a:b)
 |n > 0 = a : escolheN (n-1) b
 |otherwise = []

--Função lista de produtos

type Produto = (String, Float)

produtos :: Int -> [Produto] -> [Produto]
produtos _[] = []
produtos n(a:b)
 |n > 0 = a : produtos (n-1) b
 |otherwise = []

--Função para buscar produtos

busca :: [Produto] -> Produto -> Bool
busca [] _ = False
busca (a:b) n 
 |n == a = True
 |otherwise = busca b n

--3) Função para multiplicar o inteiro passado 

multiList :: Int->[Int]->[Int]
multiList _[] = []
multiList n(a:b) = n*a : multiList n b

--4)Função para achar maior elemento da lista

maiorList :: [Int]->Int
maiorList [a] = 0
maiorList (a:b)
 |(a > maiorList b) = a
 |otherwise = maiorList b

--5)Função para mostrar a lista invertida

invertList :: [Int]->[Int]
invertList [] = []
invertList(a:b) = (invertList b)++[a]

--6)Função para retornar o tipo e perimetro do triangulo

tipoPerimetroTri :: Int -> Int -> Int -> (String,Int)
tipoPerimetroTri a b c
 |(equilateroTri a b c) = ("equilatero", a+b+c)
 |(escalenoTri a b c) = ("escaleno", a+b+c)
 |(isocelesTri a b c) = ("isoceles", a+b+c)
 |otherwise = error "Nao e triangulo"

--7)Função para ler lista e e valor de produtos e mostrar o valor total

valorTotal :: [Produto] -> Float
valorTotal [] = 0
valorTotal ((n,v):b) = v + valorTotal b

--8)Função anterior adaptada para reber a quantidade de produto

type Produto2 = (String,Float,Float)

valorTotal2 :: [Produto2] -> Float
valorTotal2 [] = 0
valorTotal2 ((_,q,v):b) = q*v + valorTotal2 b

-- 9)Função para ler o nome do produto, valor unitário e quantidade comprada de uma lista de produtos

gastosList :: [Produto2] -> [Produto]
gastosList [] = []
gastosList ((n,v,q):b) = (n, v * q) : gastosList b

--Exercicio Funções e listcompreheison

--Função para retornar multiplos de 3

multi3 :: [Int]
multi3 = [x|x <- [1..100], mod x 3 == 0]

--Multiplos de N numeros

multiN :: Int -> [Int] -> [Int]
multiN n lista = [x|x <- lista, mod x n == 0]

--Função para calcular a soma do quadrado

somaQuad :: Int
somaQuad = sum[x^2|x <- [1..50]]

--Importação de arquivos


--polimorfismo

quantos1 :: [a] -> Int
quantos1 [] = 0
quantos1(a:b) = 1 + quantos1 b

--1)Juntar duas listas de qualquer tipo

combina :: [a] -> [b] -> [(a,b)]
combina l1 l2 = [(x,y)| x <- l1, y <- l2]

--2)N primeiros elementos de uma listta polimorfica

poliN :: [a] -> Int -> [a]
poliN _ 0 = []
poliN [] _ = []
poliN (x:s) n = x : poliN s (n-1)

--3)Retorna o ultimo elemento

ultimo :: [a] -> a
ultimo [] = error "Lista vazia"
ultimo [x] = x
ultimo (x:s) = ultimo s

--Ultimo dia

ex2 :: (Num a) => [a] -> a
ex2 [] = 0
ex2 (x:xs) = x + ex2 xs

quantos :: (Ord a) => [a] -> Int
quantos [] = 0
quantos (x:xs) = 1 + quantos xs

intervalo x y = quantos(enumFromTo x y)

-- Slide sobre funções de alta ordem

-- Mapeamento

dobro :: Float -> Float
dobro x = 2*x

mapear :: (Float -> Float) -> [Float] -> [Float]
mapear f [] = []
mapear f (x:xs) = (f x): mapear f xs

-- Filtragem

maior5 :: Int -> Bool
maior5 x
  |x>5 = True
  |otherwise = False

par :: Int -> Bool
par x
  |mod x 2 == 0 = True
  |otherwise = False

filtrar :: (Int -> Bool) -> [Int] -> [Int]
filtrar f [] = []
filtrar f (x:xs)
  |f x = x: filtrar f xs
  |otherwise = filtrar f xs

--Redução

somat :: Int -> Int -> Int
somat x y = x+y

multip :: Float -> Float -> Float
multip x y = x*y
 
reduzir :: (a -> a -> a) -> [a] -> a
reduzir f []  = error "Lista de entrada vazia"
reduzir f [x]  = x
reduzir f (x:xs)  = f x (reduzir f xs)

mapear2 :: (Num a) => (a-> a) -> [a] -> [a]
mapear2 f [] = []
mapear2 f (x:xs) = (f x): mapear2 f xs

exercicio2 :: (Num a) => [a] -> a
exercicio2 lista = reduzir (+) (mapear2 (*2) lista)

-- Exercicio 3 do slide

quadrado :: Float -> Float
quadrado x = x*x

ex3aux ::  [Float] -> Float -> [Float]
ex3aux [] _ = []
ex3aux (x:xs) y = (x/y) : ex3aux xs (y+1)

exercicio3 :: [Float] -> Float
exercicio3 lista = reduzir (+) (ex3aux (mapear quadrado lista) 1)

-- Exercicio 4
mapearP :: (Ord a) => (a-> a) -> [a] -> [a]
mapearP f [] = []
mapearP f (x:xs) = (f x): mapearP f xs

maiusc :: String -> String
maiusc lista = mapearP toUpper lista

minusc :: String -> String
minusc lista = mapearP toLower lista

exercicio4 :: [String] -> Int -> [String]
exercicio4 lista x
  |x == 1 = mapearP maiusc lista
  |otherwise = mapearP minusc lista


-- Exercicio 5 com erro em letras finais do alfabeto
cripto :: String -> Int -> String
cripto [] _ = []
cripto (x:xs) a = chr(ord x + a) : cripto xs a

-- Exercicio 5 reiniciando do inicio do alfabeto
cripto2 :: String -> Int -> String
cripto2 [] _ = []
cripto2 (x:xs) a
  |x == ' ' = ' ':cripto2 xs a
  |chr(ord x + a) > 'z' = chr(((ord x + a) - (ord 'z' +1)) + ord 'a') : cripto2 xs a
  |otherwise = chr(ord x + a) : cripto2 xs a
