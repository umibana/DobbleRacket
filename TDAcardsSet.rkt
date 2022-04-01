#lang racket

;; Para la creacion del set de cartas se haa uso del algoritmo
;; proporcionado por Micky Dore [https://mickydore.medium.com/the-dobble-algorithm-b9c9018afc52]
;;
;; El algoritmo hace uso for loops. Para la implementacion en Racket se usara recursion
;;

(define (primeraCarta elementos)
  (cond
    [(= elementos 1) (list 1)]
    [(> elementos 1) (cons elementos (primeraCarta (- elementos 1)))]))


(define (nCartas elementos i k)
  (list 1)
  (define (nCartas_k elementos)
    (cond
      [(= elementos 1) (list k)]
      [(> elementos 1) (cons (+ (* elementos i ) (+ k 1)) (nCartas_k elementos i (- k 1)))]))
  (cond
     [(= elementos 1) (list 1)]
     [(> elementos 1) (cons elementos (nCartas (- elementos 1) i k))]))


;; (cond
;;   [(= elementos 1) (list 1)]
;;   [(> elementos 1) (cons elementos (nCartas (- elementos 1)))]))
;
;
(nCartas 30 30 30)




;; Funcion de ejemplo de aleatoriedad.
(define m 2147483647)
(define a 1103515245)
(define c 12345)

(define randomFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)))



;; define (cardsSet elements numE maxC randomFn))
;;(define (dobble! elementos)
;; (primeraCarta elementos))
;;   (nCartas elementos)
;;   (n2Cartas elementos))
