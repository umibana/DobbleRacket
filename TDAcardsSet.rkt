#lang racket

;; Para la creacion del set de cartas se haa uso del algoritmo
;; proporcionado por Micky Dore [https://mickydore.medium.com/the-dobble-algorithm-b9c9018afc52]
;;
;; El algoritmo hace uso for loops. Para la implementacion en Racket se usara recursion
;;


;; Primera parte del algoritmo (creacion de primera carta)
(define (primeraCarta n)
  (cond
    [(= n 1) (list 1)]
    [(> n 1) (cons (+ n 1) (primeraCarta (- n 1)))]))


; Segunda parte del algoritmo (Creacion de n Cartas)

(define (nCartas_k n i k)
  (cond
    [(= k 1) (list k)]
    [(> k 1) (cons (+ (* n i ) (+ k 1)) (nCartas_k n i (- k 1)))]))

(define (nCartas n i k)
  (cond
   [(= i 1) (nCartas_k n n n)]
   [(> i 1) (cons i (nCartas n (- i 1) k))]))

;; (nCartas_k 5 5 5)

(primeraCarta 3)
(nCartas 4 4 4)




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
