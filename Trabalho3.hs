--Nome: Lucas Gabriel Mendes de Castro

-- 1. Escreva  uma  função  para  o  cálculo  dos  números  da  sequência  de  Fibonacci,  utilizando 
-- Haskell. 
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


-- 2. Um dos primeiros algoritmos documentados é o algoritmo para o cálculo do Maior Divisor 
-- Comum  (MDC)  de  Euclides  publicado  por  volta do  ano 300  AC.  Podemos simplificar  este 
-- algoritmo  dizendo  que  dados  dois  inteiros  A  e  B,  o  MDC  entre  eles  será dado  pelo  valor 
-- absoluto de A se B=0 e pelo MDC entre B e o resto da divisão de A por B se B>0. Escreva 
-- uma  função  para  o  cálculo  do  MDC  entre  dois  números  inteiros  positivos,  usando  o 
-- algoritmo de Euclides conforme apresentado aqui, utilizando Haskell.
mdc :: Int -> Int -> Int
mdc 0 a = a 
mdc a b = mdc (b `mod` a) a

-- 3. Escreva uma função recursiva que dado um número inteiro n, devolva a soma dos dígitos 
-- deste  número.  Exemplo:  dado  1234  a  função  deverá  devolver  10.  Utilizando  Haskell  e 
-- recursividade. 
soma :: Int -> Int
soma 0 = 0
soma x = (x `mod` 10) + soma(x `div` 10)

-- 4. Escreva  uma  função  que  devolva  a  soma  de  todos  os  números  menores  que  10000  que 
-- sejam múltiplos de 3 ou 5.
somaTodos :: Int -> Int -> Int
somaTodos a xs | a >= 10000 = xs
                    | mod a 3 == 0 || mod a 5 == 0 = a + somaTodos (a + 1) xs
                    | otherwise = somaTodos (a + 1) xs

-- 5. Escreva  uma  função que,  recebendo  uma  lista  de  inteiros,  apresente  a  diferença  entre a soma dos quadrados e o quadrado da soma destes inteiros, usando recursividade. 
diferanca_aux :: [Int] -> Int -> Int
diferanca_aux a b | null (tail a) = head a^b
             | otherwise = (head a)^b + diferanca_aux (tail a)b
diferenca :: [Int] -> Int
diferenca a  = (diferanca_aux a 2) - (diferanca_aux a 1)^2

-- 6. O Crivo de Eratóstenes não é o melhor algoritmo para encontrar números primos. Crie uma função que implemente o Crivo de Euler (Euler’s Sieve) para encontrar todos os números primos menores que um determinado inteiro dado. 
divisores :: Int -> [Int]
divisores 0 = [1]
divisores n = [x | x <- [1,2..n], n `mod` x == 0]
crivo :: Int -> [Int]
crivo n = [x | x <- [1,2..n], length(divisores x) == 2 ]

-- 7. Nem  só  de  Fibonacci  vivem  os  exemplos  de  recursão.  Escreva  uma  função  que  devolva todos os números de uma sequência de Lucas ----- (2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123) menores que um inteiro dado. 
lucas_aux :: Int -> Int
lucas_aux (0) = 2
lucas_aux (1) = 1
lucas_aux n = lucas_aux (n - 2) + lucas_aux (n - 1)

lucas :: Int -> [Int]
lucas n = [lucas_aux (x) | x <- [0..n]]

-- 8. Escreva uma função, chamada aoContrario em Haskel para reverter uma lista. Dado [1,2,3] 
-- devolva [3,2,1]. 
aoContrario :: [Int] -> [Int]
aoContrario x | length x == 1 = x
              | otherwise = aoContrario (tail x) ++ [head x]

-- 9. Escreva uma função chamada somaRecursiva que recebe dois valores inteiros e devolve o produto destes valores sem usar o operador de multiplicação.  
somaRecursiva :: Int -> Int -> Int
somaRecursiva x y | y == 1 = x
                  | otherwise = x + somaRecursiva x (y-1)
                  
-- 10. Escreva uma função chamada comprimento que receba uma lista de  inteiros e devolva o comprimento desta lista. Observe que você não pode usar nenhuma função que já calcule o comprimento de uma lista. 
comprimento :: [Int] -> Int -> Int
comprimento x y | null (tail x) = y +1
                | otherwise = comprimento (tail x) (1 + y)

main = do
putStrLn("Ex. 1: entrada:6 >> resultado:" ++ show(fib 6))
putStrLn("Ex. 2: entrada:6 3 >> resultado:" ++ show(mdc 6 3))
putStrLn("Ex. 3: entrada:12345 >> resultado:" ++ show(soma 12345))
putStrLn("Ex. 4: entrada:0 0 >> resultado:" ++ show(somaTodos 0 0))
putStrLn("Ex. 5: entrada:[1,2,3] >> resultado:" ++ show(diferenca [1,2,3]))
putStrLn("Ex. 6: entrada:21 >> resultado:" ++ show(crivo 21))    
putStrLn("Ex  7: entrada:11 >> resultado:" ++ show(lucas 11))
putStrLn("Ex. 8: entrada:[1,2,3,4] >> resultado:" ++ show(aoContrario [1,2,3,4]))
putStrLn("Ex. 9: entrada:2 3 >> resultado:" ++ show(somaRecursiva 2 3))
putStrLn("Ex. 10: entrada:150 1.50 >> resultado:" ++ show(comprimento [1,2,3,4,5,6,7] 0))
