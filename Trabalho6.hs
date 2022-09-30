import Data.Char

-- Lucas Gabriel Mendes de Castro
-- 1. Usando List Comprehension escreva uma função, chamada divisoresDeN, que devolva uma lista dos divisores de um número dado.
divisoresDeN :: Int -> [Int]
divisoresDeN n = [x | x <- [1..n], (n `mod` x) == 0]

-- 2. Usando  List Comprehension  escreva  uma  função, chamada  contaCaractere,  que  conte  a ocorrência de um caractere específico, em uma string dada.
contaCaractere :: String -> Char -> Int
contaCaractere t c = length [x | x <- t, (toLower x) == (toLower c)]

-- 3. Usando List Comprehension escreva uma função, chamada dobroNaoNegativo, que devolve o dobro dos valores dos elementos não negativos da lista de inteiros dada. 
dobroNaoNegativo :: [Int] -> [Int]
dobroNaoNegativo x = [n*2 | n <- x, n >= 0]

-- 4. Usando List Comprehension escreva uma função, chamada pitagoras, que devolva uma lista de triplas, não repetidas, contendo os lados dos triângulos retângulos possíveis de serem construídos por inteiros entre 1 e um número inteiro dado.
pitagoras :: Int -> [(Int, Int, Int)]
pitagoras n = [(x,y,z) | x <-[1..n], y <-[1..n], z <- [1..n], ((x^2) + (y^2)) == (z^2), z > x, z > y, y > x]


-- 5. Números  perfeitos  são  aqueles  cuja  soma  dos seus  divisores  é  igual  ao  próprio  número. Usando List Comprehension escreva uma função, chamada numerosPerfeitos, que devolva uma lista contendo todos os números perfeitos menores que um número dado. Lembre-se que você já tem uma função que devolve uma lista dos divisores de um número dado.
numerosPerfeitos :: Int -> [Int]
numerosPerfeitos n = [x | x <- [1..n], (sum (init (divisoresDeN x))) == x]

-- 6. Usando List Comprehension escreva uma função, chamada produtoEscalar, que devolva o produto escalar entre duas listas de inteiros. Lembre-se, existem as funções fst, snd e zip no prelude que podem ser úteis.
produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar x y = (sum [i*k | (i, k) <- zip x y])

-- 7. Usando  List Comprehension  escreva  uma  função, chamada  primeirosPrimos,  que  devolva uma lista contendo, os n primeiros números primos a partir do número 2.
primeirosPrimos :: Int -> [Int]
primeirosPrimos valor = takeWhile (\x -> length [n | n <- [2..(x-1)], length((divisoresDeN n)) == 2] < valor ) [n | n <- [2..], length((divisoresDeN n)) == 2]

-- 8. Usando  List Comprehension  escreva  uma  função,  chamada  paresOrdenados,  que  devolva uma  lista  de  par ordenados  contendo  uma  potência  de  2  e  uma potência de  3  até  um determinado número dado. Observe que estes números podem ser bem grandes.
paresOrdenados :: Int -> [(Double, Double)]
paresOrdenados n = [(2^x, 3^x) | x <- [0..n]]

main = do
putStrLn("Funcao1: entrada: 8 >> resultado:" ++ show (show(divisoresDeN 8)))
putStrLn("Funcao2: entrada: Fofoqueiro >> resultado:" ++ show (show(contaCaractere "Fofoqueiro" 'o')))
putStrLn("Funcao3: entrada: [-5,-4,-3,-2,-1,0,1,2] >> resultado:" ++ show (show(dobroNaoNegativo [-5,-4,-3,-2,-1,0,1,2])))
putStrLn("Funcao4: entrada: 10 >> resultado:" ++ show (show(pitagoras 10)))
putStrLn("Funcao5: entrada: 500 >> resultado:" ++ show (show(numerosPerfeitos 500)))
putStrLn("Funcao6: entrada: [2,2,2] [2,2,2] >> resultado:" ++ show (show(produtoEscalar [2,2,2] [2,2,2])))
putStrLn("Funcao7: entrada: 5 >> resultado:" ++ show (show(primeirosPrimos 5)))
putStrLn("Funcao8: entrada: 10 >> resultado:" ++ show (show(paresOrdenados 10)))
