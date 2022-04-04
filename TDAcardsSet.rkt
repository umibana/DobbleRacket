;; Para la creacion del set de cartas se haa uso del algoritmo
;; proporcionado por Micky Dore [https://mickydore.medium.com/the-dobble-algorithm-b9c9018afc52]
;;
;; El algoritmo hace uso for loops. Para la implementacion en Racket se usara recursion
#lang racket

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


(define (n2Cartas_k n i j k)
  (cond
    [(= k 1) (cons (+ i 1) (list (n2Algoritmo n i j k)))]
    [(> k 1) (cons (n2Algoritmo n i j k ) (n2Cartas_k n i j (- k 1)))]))

; Segundo loop
(define (n2Cartas_j n i j k)
  (cond
    [(= j 0) (cons (+ i 1)(n2Cartas_k n i j n))]
    [(> j 0) (cons (n2Cartas_j n i (- j 1) n) (+ i 1))]))

; Primer loop
(define (n2Cartas n i j k)
  (cond
    [(= i 0) (cons 1 (n2Cartas_j n (+ i 1) n n))]
    [(> i 0) (cons (n2Cartas n (- i 1) j k) (n2Cartas_j n i n n))]))

(flatten(n2Cartas 3 3 3 3))




;; (flatten(append(reverse(primeraCarta 3))(reverse(nCartas 3 3 3))))



;; (1 1 11 9
;;  7 1 2 12
;;  13 11 2 13
;;  10 7 2 4 11
;;  12 13 3 12
;;  8 7 3 3
;;  13 11 12 4
;;  11 9 7 4
;;  2 12 13 11)


;; Funcion de ejemplo de aleatoriedad.
;; (define m 2147483647)
;; (define a 1103515245)
;; (define c 12345)

;; (define randomFn (lambda (xn)
;;                    (modulo (+ (* a xn) c) m))))



;; define (cardsSet elements numE maxC randomFn))
;;(define (dobble! n)
;; (primeraCarta n))
;;   (nCartas n)
;;   (n2Cartas n))
