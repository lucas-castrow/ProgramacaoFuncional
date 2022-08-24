-- 1. Escreva uma função chamada soma1 que recebe um inteiro como argumento e retorna um 
-- inteiro uma unidade maior que a entrada.  
soma1 :: Integer -> Integer
soma1 x =  x + 1

-- 2. Escreva  uma  função  chamada  sempre  que,  não  importando  o  valor  de  entrada,  devolva 
-- sempre zero. Observe que neste caso a entrada pode ser de qualquer tipo. 
sempre :: Integer -> Integer
sempre x = 0

-- 3. Escreva  uma  função  chamada  treco  que  receba  três  valores  em  ponto  flutuantes  com 
-- precisão dupla e retorne o resultado da soma dos dois primeiros multiplicado pelo terceiro. 
treco :: Float -> Float -> Float -> Float
treco x y z = (x+y) * z 

-- 4. Escreva uma função chamada resto que devolva o resto de uma divisão entre dois números 
-- inteiros. 
resto :: Int -> Int -> Int
resto x y = x `mod` y

-- 5. Escreva uma função chamada precoMaior que devolva o maior valor entre quatro valores 
-- monetários.
precoMaior :: Float -> Float -> Float -> Float -> Float
precoMaior a b c d = maximum[a,b,c,d]

-- 6. Escreva uma função chamada impar que devolva True, sempre que o resultado do produto 
-- de dois números inteiros for ímpar.  
impar :: Int -> Int -> Bool
impar a b | mod (a * b) 2 == 0 = False | otherwise = True -- É ímpar

-- 7. Escreva uma função em Haskell que receba números reais (double) e devolva o resultado 
-- da equação 𝑥2 +𝑦/2 +𝑧. 
equacao :: Double -> Double -> Double -> Double
equacao a b c = (a*a) + b/2 + c

-- 8. Escreva uma função em Haskell chamada diagnostico que receba o peso do aluno e imprima 
-- um  diagnóstico  de  obesidade,  segundo  a  tabela  que  pode  ser  encontrada  no  link: 
-- Sobrepeso,  obesidade  e  obesidade  mórbida:  entenda  a  diferença  entre  os  três  termos 
-- (cuidadospelavida.com.br).  Observe  que  este  diagnóstico  é  meramente  estatístico  e  não 
-- tem nenhum valor real, está sendo usado nesta questão apenas para a definição das faixas. 
-- Todo e qualquer diagnóstico deve ser feito por um profissional médico.  
diagnostico :: Float -> Float -> String
diagnostico x y | x / (y * y) < 17 = "Muito abaixo do peso"
  | x / (y * y) >= 17 && x / (y * y) <= 18.49 = "Abaixo do peso"
  | x / (y * y) >= 18.5 && x / (y * y) <= 24.99 = "Normal"
  | x / (y * y) >= 25 && x / (y * y) <= 29.99 = "Sobrepeso" | x / (y * y) >= 30 && x / (y * y) <= 34.99 = "Obesidade Ieve" | x / (y * y) >= 35 && x / (y * y) <= 39.99 = "Obesidade Severa"
  | otherwise = "Obesidade morbida"

-- 9. Escreva uma função em Haskell chamada bissexto que receba um ano e devolva True se o 
-- ano for bisexto sabendo que anos bissextos obedecem a seguinte regra:  
bissexto :: Int -> Bool
bissexto x | mod x 4 == 0 && (mod x 100 /= 0 || mod x 400 == 0) = True
           | otherwise = False
main = do   
putStrLn("\nEx 1 soma1: entrada: 3, resultado: "++ show (soma1 3))
putStrLn("\nEx 2 sempre: entrada: 100, resultado: "++ show (sempre 100))
putStrLn("\nEx 3 treco: entrada: 2.0 4.0 2.0, resultado: "++ show (treco 2.0 4.0 2.0))
putStrLn("\nEx 4 resto: entrada: 4 2, resultado: "++ show (resto 4 2))
putStrLn("\nEx 5 precoMaior: entrada: 2.9 4.0 6.7 3.4, resultado: "++ show (precoMaior 2.9 4.0 6.7 3.4))
putStrLn("\nEx 6 impar: entrada: 3 53, resultado: "++ show (impar 3 5))
putStrLn("\nEx 7 equacao: entrada: 2 6 3, resultado: "++ show (equacao 2 6 3))
putStrLn("\nEx 8 diagnostico: entrada: 70 1.70, resultado: "++ show (diagnostico 70 1.70))
putStrLn("\nEx 9 bissexto: entrada: 2000, resultado: "++ show (bissexto 2000))
