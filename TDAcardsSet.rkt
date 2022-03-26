#lang racket

;; Para la creacion del set de cartas se hara uso del algoritmo
;; proporcionado por Micky Dore [https://mickydore.medium.com/the-dobble-algorithm-b9c9018afc52]
;;
;;
(define elementCards 10)

;; (define (cardsSet elementos n_elementos max_cartas randomFn)
;;   (for ([i (in-range 1 elementos)])
;;     (cons i '(1))))


;; Crea la primera carta
(define (primeraCarta elementos)
  (reverse
   (for/fold ([result '()])
            ([i (in-range 1 elementos)])
    (cons i result))))

(define (nCartas elementos)
  (reverse
   (for*/fold ([result '()])  ;; for* = nested loops
       ([j (in-range 1 elementos)]      ;
        [k (in-range 5 10)])
     (cons (+ (* k elementos) (+ k 1) ) result)
     (cons j result))))


(define (n2Cartas elementos)
 (reverse
  (for*/fold ([result '()])
             ([j (in-range 1 elementos)]
              [k (in-range 1 elementos)])
    (cons (* (- k 1) (+ elementos 2 elementos)(modulo(+ (*(- j 1) (- k 1)) (- j 1))))result)
    (cons j result))))



(nCartas 3)
