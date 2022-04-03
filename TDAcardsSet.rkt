#lang racket

;; Para la creacion del set de cartas se haa uso del algoritmo
;; proporcionado por Micky Dore [https://mickydore.medium.com/the-dobble-algorithm-b9c9018afc52]
;;
;; El algoritmo hace uso for loops. Para la implementacion en Racket se usara recursion
;;


;; Primera parte del algoritmo (creacion de primera carta)
(define (primeraCarta n)
  (cond
    [(= n 0) (list 1)]
    [(> n 0) (cons (+ n 1) (primeraCarta (- n 1)))]))


; Segunda parte del algoritmo (Creacion de n Cartas)

(define (nCartas_k n i k)
  (cond
    [(and (= i 1)(= k 1)) (list (+ (* n i ) (+ k 1)))]
    [(= k 0) (list 1)]
    [(> k 0) (cons (+ (* n i ) (+ k 1)) (nCartas_k n i (- k 1)))]))

(define (nCartas n i k)
  (cond
    [(= i 1) (cons 1 (nCartas_k n i n))]
    [(> i 1) (cons (nCartas n (- i 1) k) (nCartas_k n i n))]))

;; (nCartas_k 5 5 5)

(primeraCarta 3)
(nCartas 3 3 3)




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
