;; Para la creacion del set de cartas se haa uso del algoritmo
;; proporcionado por Micky Dore [https://mickydore.medium.com/the-dobble-algorithm-b9c9018afc52]
;;
;; El algoritmo hace uso for loops. Para la implementacion en Racket se usara recursion
#lang racket

;;Funcion de ejemplo de aleatoriedad.
(define m 2147483647)
(define a 1103515245)
(define c 12345)

(define randomFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)))


;; Primera parte del algoritmo (creacion de primera carta)
(define (primeraCarta n)
  (cond
    [(= n 0) (list 1)]
    [(> n 0) (cons (+ n 1) (primeraCarta (- n 1)))]))

; Segunda parte del algoritmo (Creacion de n Cartas)
; Se crea una funcion nCartas y nCartas_j (Auxiliar)
; La funcion nCartas_j sera llamada por la funcion principal (nCartas) recursivamente
; Y generara los simbolos para cada carta
; nCartas, en cambio generara la carta y recursivamente llamando a nCartas_j, generara los simbolos

; Defino la operacion del algoritmo para no colocarlo dentro de la funcion
(define nAlgoritmo
  (lambda (n i j)(+ (* n i ) (+ j 1))))

(define (nCartas_j n i j)
  (cond
    [(and (= i 1)(= j 1)) (list (nAlgoritmo n i j))]
    [(= j 0) (list 1)]
    [(> j 0) (cons (nAlgoritmo n i j) (nCartas_j n i (- j 1)))]))

(define (nCartas n i j)
  (cond
    [(= i 1) (cons 1 (nCartas_j n i n))]
    [(> i 1) (cons (nCartas n (- i 1) j) (nCartas_j n i n))]))

; Tercera parte del algoritmo
; Define la operacion del algoritmo para que no quede tan largo dentro de la funcion
(define n2Algoritmo
  (lambda (n i j k) (+(+ (* n (- k 1)) (+ n 2))(modulo (+ (* (- i 1) (- k 1))(- j 1) )n))))
; loop k
(define (n2Cartas_k n i j k)
  (cond
    [(= k 1) (cons (n2Algoritmo n i j k) (list (+ i 1)))]
    [(> k 1) (cons (n2Algoritmo n i j k ) (n2Cartas_k n i j (- k 1)))]))
; loop j
(define (n2Cartas_j n i j k)
  (cond
    [(= j 1) (n2Cartas_k n i j n)]
    [(> j 1) (cons (n2Cartas_j n i (- j 1) k) (n2Cartas_k n i j k))]))
; loop i
(define (n2Cartas n i j k)
  (cond
    [(= i 1) (n2Cartas_j n i n n)]
    [(> i 1) (cons (n2Cartas n (- i 1) n n) (n2Cartas_j n i n n))]))


;; Funcion para separar la lista segun n elementos
(define (split-by lst n)
   (if (not (empty? lst))
       (cons (take lst n) (split-by (drop lst n) n))
       '()))

(define creacionMazo (lambda (ordenPlano)
                       (append(flatten(primeraCarta ordenPlano))
                        (append(flatten(nCartas ordenPlano ordenPlano ordenPlano))
                         (flatten(n2Cartas ordenPlano ordenPlano ordenPlano ordenPlano))))))

(define ordenarMazo (lambda (ordenPlano)
                      (split-by (creacionMazo ordenPlano) (+ ordenPlano 1))))


(ordenarMazo 7)







;; define (cardsSet elements numE maxC randomFn))
;;(define (dobble! n)
;; (primeraCarta n))
;;   (nCartas n)
;;   (n2Cartas n))
