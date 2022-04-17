;; Para la creacion del set de cartas se haa uso del algoritmo
;; proporcionado por Micky Dore [https://mickydore.medium.com/the-dobble-algorithm-b9c9018afc52]
;;
;; El algoritmo hace uso for loops. Para la implementacion en Racket se usara recursion
#lang racket

(provide cardsSet cardsSet->string)
;;Funcion de ejemplo de aleatoriedad.
(define m 2147483647)
(define a 1103515245)
(define c 12345)

(define randomFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)))

;; Constructores
;;
;; Descripicion: Generar la primera carta del mazo
;; Dominio: Int
;; Recorrido: Lista
;; Tipo de Recursion: Recursion natural
(define (primeraCarta n)
  (cond
    [(= n 0) (list 1)]
    [(> n 0) (cons (+ n 1) (primeraCarta (- n 1)))]))

;; Descripcion: Segunda parte del algoritmo (Creacion de n Cartas)
;; Se crea una funcion nCartas y nCartas_j (Auxiliar) para la generacion de cartas
;; y la funcion nAlgoritmo para el algoritmo para que los simbolos no se repitan mas de una vez
;; La funcion nCartas_j sera llamada por la funcion principal (nCartas) recursivamente
;; Y generara los simbolos para cada carta
;; Dominio: Int
;; Recorrido: Lista de listas
;; Tipo de Recursion: Recursion natural

(define (nCartas n i j)

  (define nAlgoritmo
    (lambda (n i j)(+ (* n i ) (+ j 1))))

  (define (nCartas_j n i j)
    (cond
      [(and (= i 1)(= j 1)) (list (nAlgoritmo n i j))]
      [(= j 0) (list 1)]
      [(> j 0) (cons (nAlgoritmo n i j) (nCartas_j n i (- j 1)))]))

  (cond
    [(= i 1) (cons 1 (nCartas_j n i n))]
    [(> i 1) (cons (nCartas n (- i 1) j) (nCartas_j n i n))]))

;; Descripcion: Tercera parte del algoritmo (Creacion de n Cartas)
;; Se crean 4 funciones, n2cartas,n2cartas_j,n2cartas_j para la generacion de cartas
;; y la funcion n2Algoritmo para evitar que los simbolos se repitan
;; La funciones n2Cartas llamara recursivamente a n2Cartas_j, y n2Cartas_j llamara recursivamente
;; a n2Cartas_k para generar los simbolos
;; Dominio: int
;; Recorrido: Lista de listas
;; Tipo de Recursion: Recursion natural
(define (n2Cartas n i j k)

 (define n2Algoritmo      ;; Algoritmo para la creacion de cartas
   (lambda (n i j k) (+(+ (* n (- k 1)) (+ n 2))(modulo (+ (* (- i 1) (- k 1))(- j 1) )n))))

 (define (n2Cartas_k n i j k) ;; loop k
   (cond
     [(= k 1) (cons (n2Algoritmo n i j k) (list (+ i 1)))]
     [(> k 1) (cons (n2Algoritmo n i j k ) (n2Cartas_k n i j (- k 1)))]))

 (define (n2Cartas_j n i j k) ;; loop j
   (cond
     [(= j 1) (n2Cartas_k n i j n)]
     [(> j 1) (cons (n2Cartas_j n i (- j 1) k) (n2Cartas_k n i j k))]))

 (cond  ;; loop i
     [(= i 1) (n2Cartas_j n i n n)]
     [(> i 1) (cons (n2Cartas n (- i 1) n n) (n2Cartas_j n i n n))]))


;; Descripcion: Funcion para poder dejar todo el mazo en una sola lista
;; Dominio: Int
;; Recorrido: Lista
;; Tipo de Recursion: No se hace uso de recursion
(define aplanarMazo
  (lambda (ordenPlano)
    (append(flatten(reverse(primeraCarta ordenPlano)))
           (append(flatten(nCartas ordenPlano ordenPlano ordenPlano))
                  (flatten(n2Cartas ordenPlano ordenPlano ordenPlano ordenPlano))))))

;; Descripcion: Funcion para poder tomar la lista de carta
;; y separarlas segun el numero de simbolos por carta indicado
;; Dominio: Lista e int
;; Recorrido: Lista de listas
;; Tipo de recursion: Recursion Natural
(define (ordenarEn mazo n)
  (cond
    [(not (empty? mazo)) (cons (take mazo n) (ordenarEn (drop mazo n) n))]
    [else '()]))

;; Descripcion: Funcion para facilitar uso de ordenarEn
;; Dominio: Int
;; Recorrido: Lista de listas
;; Tipo de Recursion: No se hace uso de recursion
(define ordenarMazo (lambda (ordenPlano)
                            (ordenarEn (aplanarMazo ordenPlano) (+ ordenPlano 1))))

;; Descripcion: Funcion que buscara un elemento en una lista y lo remplazara por el indicado
;; Dominio: String o Int, Lista
;; Recorrido: Lista
;; Tipo de Recursion: Recursion natural

(define (remplazarCartas buscar remplazo carta)
  (cond
    [(null? carta) '()]
    [(list? (car carta))
     (cons (remplazarCartas buscar remplazo (cdr carta))
           (remplazarCartas buscar remplazo (cdr carta)))]
    [(equal? buscar (car carta))
     (cons remplazo
           (remplazarCartas buscar remplazo (cdr carta)))]
    [else (cons (car carta)
                (remplazarCartas buscar remplazo (cdr carta)))]))

;; Descripcion: Tomara los simbolos indicados por el usuario y los pondra en el mazo generado
;; Dominio: String o int, lista de listas, int
;; Recorrido: Lista de listas
;; Tipo de Recursion: Recursion natural

(define remplazarMazo
  (lambda (simbolos mazo i)
    (cond
      [(= (+ i 1) (length simbolos))
       (remplazarCartas (+ i 1) (list-ref simbolos i) mazo)]
      [else (remplazarMazo simbolos
                           (remplazarCartas (+ i 1) (list-ref simbolos i) mazo)
                           (+ i 1))])))

;; Descripcion: Funcion para facilitar uso de ordenarMazo
;; Dominio: Int
;; Recorrido: Lista de listas
;; Tipo de Recursion: No se usa recursion

(define armarMazo
  (lambda (elementos)
    (cond
      [(integer? elementos) (ordenarMazo elementos)]
      [else (error "error: ingrese elementos validos")])))

;; Descripcion: Funcion que generara el mazo de cartas para jugar Dobble.
;; Dominio: Lista o int, int, int, Funcion
;; Recorrido: Lista de listas
;; Tipo de Recursion: No se hace uso de recursion

(define (cardsSet elementos numE maxC)
  (cond
    [(list? elementos)
     (cond
       [(or (> maxC (+(* numE (- numE 1))))(negative? maxC))
        (ordenarEn (remplazarMazo elementos (aplanarMazo (- numE 1))0) numE)]
       [else
        (take(ordenarEn (remplazarMazo elementos (aplanarMazo (- numE 1))0) numE)maxC)])]
    [else
     (cond
       [(or (> maxC (+(* numE (- numE 1))))(negative? maxC))
        (armarMazo (- numE 1))]
       [else (take (armarMazo (- numE 1))maxC)])]))

;; Descripcion: Funcion que verificara si el mazo dado es valido para jugar Dobble
;;
;; Dominio: Lista de listas
;; Recorrido: Booleano y string
;; Tipo de Recursion:

(define (dobble? mazo)
  (cond
    [(and
      (= (numCards mazo) (+ 1 (*(length (car mazo)) (- (length (car mazo)) 1)))) ;; Reviso si tiene la cantidad de cartas necesarias
      (empty? (set-subtract (cardsSet 0 (length (car mazo))) mazo))) (printf "Conjunto Valido")] ;; reviso que no se repitan
    [else printf "Conjunto no valido"]))

;; Descripcion: Funcion que entregara el numero de cartas en el mazo ingresado
;; Dominio: Lista de listas (cardsSet)
;; Recorrido: Int
;; Tipo de Recursion: No se hace uso de recursion

(define (numCards mazo) (length mazo))

;; Descripcion: Funcion que entregar la n-ava carta del mazo
;; Dominio: Lista de listas, int
;; Recorrido: lista
;; Tipo de Recursion: No se hace uso de recursion
(define (nthCard mazo nth) (list-ref mazo nth))

;; Descripcion: Funcion que entrega el numero de cartas totales necesarias para armar un mazo de Dobble
;; valido recibiendo una carta de muestra
;; Dominio: Lista
;; Recorrido: Int
;; Tipo de Recursion: No se hace uso de recursion

(define (findTotalCards carta)
  (printf "Se necesitan ~a cartas para un mazo válido\n"
          (numCards
           (cardsSet 0 (length carta)))))

;; Descripcion: Funcion que entrega el numero de simbolos totales necesarios para armar un mazo de Dobble
;; valido recibiendo una carta de muestra
;; Dominio:
;; Recorrido:
;; Tipo de Recursion:

(define (requiredElements carta)
  (printf "Se necesitaran ~a elementos en total para un conjunto válido\n"
          (length
                 (cardsSet 0 (length carta)))))

;; Descripcion: Encuentra las cartas que faltan para armar un mazo valido de dobble dado un conjunto de muestra
;; Dominio: Lista de listas (cardsSet)
;; Recorrido: Lista de listas
;; Tipo de Recursion: No se hace uso de recursion

;; Falta implementar correctamente
(define (missingCards mazo)
  (set-subtract (cardsSet 0 (length (nthCard mazo 0))) mazo))


;; Descripcion: Transformara el mazo a una representacion en strings
;; Dominio: Lista de listas (cardsSet)
;; Recorrido: String
;; Tipo de Recursion: Recursion Natural
(define (cardsSet->string mazo [i 1])
  (cond
    [(null? mazo) (printf "Mazo Actual")]
    [else (printf "Carta ~v: ~s\n" i (string-join (map ~a (car mazo)) " "))
          (cardsSet->string (cdr mazo ) (+ i 1))]))


(cardsSet (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n") 3 -1)
