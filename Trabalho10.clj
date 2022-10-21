; Lucas Gabriel Mendes de Castro

; 1. Na  aula  disponível  em:  https://replit.com/@frankalcantara/ClojureAula2?v=1  foram destacadas diversas funções (expressões), como funções de primeira ordem, disponíveis em Clojure.  Sua  primeira  tarefa  será  descrever  cada  uma  destas  funções  e  apresentar dois exemplos  de  uso  de  cada  uma  delas.  Lembre-se  os  exemplos  precisam  ser  utilizados  de forma que o resutado da função possa ser visto no terminal.  

; assoc -> é uma função que é ligada a um map que retorna um novo map de mesmo tipo que contem o mapeamento de chave e valor.
(println "Ex1 - assoc")
(println "Func 1: entrada: [2 4 6] 2 10 >> resultado: "(assoc [2 4 6] 2 10))
(println "Func 2: entrada: [1 3 5] 3 10 >> resultado: "(assoc [1 3 5] 3 10))
; dissoc -> é uma função que que é ligada a um map que retorna um novo map que não contém um mapemanto por chave. 
(println "Ex2 - dissoc")
(println "Func 1 entrada: {:a 2 :b 4 :c 6} :b; resultado: "(dissoc {:a 2 :b 4 :c 6} :b))
(println "Func 2 entrada: {:a 1 :b 3 :c 5} :c :b; resultado: "(dissoc  {:a 1 :b 3 :c 5} :c :b))
; range -> é uma função que cria uma sequencia de valores que se inicia em 0 e vai até um determinado valor dado, podendo passar dois parametros no qual o primeiro seria o inicio dos valores e o segundo o término.
(println "Ex3 - range")
(println "Func 1 entrada: 10; resultado: "(range 10))
(println "Func 2 entrada: 10 20; resultado: "(range 10 20))
; map -> é uma função que retorna uma sequencia que consiste no resultado da aplicação de f ao conjunto dos primeiros itens de cada col, seguido pela aplicação de f ao conjunto de segundos itens em cada coll.
(println "Ex4 - map")
(println "Func 1 entrada: [10 3 8] [11 13 5] >> resultado: "(map + [10 3 8] [11 13 5]))
(println "Func 2 entrada: [10 2 8] [4 4 4] >> resultado: "(map * [10 2 8] [4 4 4]))

; inc -> é uma função que retorna um número maior do que um número dado.
(println "Ex5 - inc")
(println "Func 1 entrada: 15 >> resultado "(inc 15))
(println "Func 2 entrada: [2 4 6] >> resultado: "(map inc [2 4 6]))

; filter -> é uma função que retorna uma sequencia do item passado como parametro para os quais retorna logical true. 
(println "Ex6 - filter")
(println "Func 1 entrada: 15 >> resultado: " (filter even? (range 15)))
(println "Func 2 entrada: 15 >> resultado: " (filter odd? (range 15)))

; into -> é uma função que retorna uma saida onde ocorreu o uso de todos os itens passados no parametro.
(println "Ex7 - into")
(println "Func 1 entrada: 1 2, 3 4} >>  resultado: "(into [] {1 2, 3 4}))
(println "Func 2 entrada: [1 2 3] '(4 5 6) >> resultado: "(into [1 2 3] '(4 5 6)))

; nth -> é uma função que retorna o valor no indíce passado como parametro.
(println "Ex8 - nth")
(def lista [1, 2, 3, 4])
(println "Func 1 entrada: 2 >> resultado: " (nth lista 2))
(println "Func 2 entrada: 3 >> resultado: " (nth lista 3))

; conj -> é uma função que retorna a adição de valores passados como parametro em uma coleção.
(println "Ex9 - conj")
(println "Func 1 entrada: [1 2 3] 4 >> resultado: " (conj [1 2 3] 4))
(println "Func 2 entrada: [1 2] 3 4 >> resultado: " (conj [1 2] 3 4))

; sort -> é uma função que dada uma coleção o retona ordenada como padrão de forma crescente.
(println "Ex10 - sort")
(println "Func 1 entrada: [3 1 2 4] >> resultado: " (sort [3 1 2 4]))
(println "Func 2 entrada: [b c a] >> resultado: " (sort ["b" "c" "a"]))

; partition-by -> é uma função que retorna uma sequência de partições de uma coleção dada.
(println "Ex11 - partition-by")
(println "Func 1 entrada: [1 1 1 2 2 3 3] >> resultado: " (partition-by odd? [1 1 1 2 2 3 3]))
(println "Func 2 entrada: [1 1 1 2 2 3 3] >> resultado: " (partition-by even? [1 1 1 2 2 3 3]))

; empty -> é um função que retorna true caso a coleção passada como parametro esteja vazia e false para caso não esteja. 
(println "Ex12 - empty")
(println "Func 1 entrada: () >> resultado: " (empty? ()))
(println "Func 2 entrada: '(1) >> resultado: " (empty? '(1)))

; count -> é uma função que retorna o retorna o numero de itens de uma coleção.
(println "Ex13 - count")
(println "Func 1 entrada: []; resultado: " (count []))
(println "Func 2 entrada: [1 2 3]; resultado: " (count [1 2 3]))

; char - é uma função que retorna o char equivalente índice dado. 
(println "Ex14 - char")
(println "Func 1 entrada: 97; resultado: " (char 97))
(println "Func 2 entrada: [65 66 67 68]; resultado: " (map char [65 66 67 68]))

; 2. Utilizando a linguagem Clojure, crie uma função chamada ehPrimo que receba um inteiro e devolva True caso este inteiro seja verdadeiro e False caso contrário. 

(defn ehPrimo? [valor] (
  loop [atual 1 lista []]
       (if (= atual (+ valor 1)) (if (= (count lista) 2) true false)
       (recur (inc atual) (if (zero? (mod valor atual)) (conj lista atual) lista)))
                   
))

; 3. Utilizando  a  linguagem  Clojure,  crie  uma  função  chamada  fatoresPrimos  que  receba  um inteiro e devolva uma lista dos seus fatores primos. Decomposição em fatores primos é uma tarefa fundamental da aritmética.
(defn isPrimo [x n]
  (if (= x n) true (if (= (mod x n) 0) false (isPrimo x (+ n 1)))))

(defn nextPrimoDivisor [x n]
  (if (false? (isPrimo n 2)) 
    (nextPrimoDivisor x (+ n 1))
    (if (= (mod x n) 0)
      n
      (nextPrimoDivisor x (+ n 1)))))

(defn fatoresPrimos [x]
  (if (true? (isPrimo x 2)) [x] (concat [(nextPrimoDivisor x 2)] (fatoresPrimos (/ x (nextPrimoDivisor x 2))))))

; 4. Utilizando  a  linguagem  Clojure,  crie  uma  função  chamada  todosPrimos  que  receba  dois valores inteiros e devolve todos os números primos que existam entre estes dois valores.
(defn todosPrimos [x y] (
   loop [lista (range x (+ y 1)) lista2 []]
      (if (empty? (rest lista)) lista2 (recur (rest lista) (if (ehPrimo? (nth lista 0)) (conj lista2 (nth lista 0)) lista2)))
))

; 2
(println "Func 2: entrada: 12 >> resultado: " (str (ehPrimo? 12)))
(println "Func 2: entrada: 27 >> resultado: " (str (ehPrimo? 27)))
; 3
(println "Func 3: entrada: 450 >> resultado: " (fatoresPrimos 450))
; 4
(println "Func 4: entrada: 20 500 >> resultado: " (str (todosPrimos 20 500)))
