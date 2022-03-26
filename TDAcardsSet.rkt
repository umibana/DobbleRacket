#lang racket

;; Para la creacion del set de cartas se hara uso del algoritmo
;; proporcionado por Micky Dore [https://mickydore.medium.com/the-dobble-algorithm-b9c9018afc52]
;;

;; Crea la primera carta
(define (primeraCarta elementos)
  (reverse
   (for/fold ([result '()])
            ([i (in-range 1 elementos)])
    (cons i result))))

;; Crea un n numero de cartas

(define (nCartas elementos)
  (reverse
   (for*/fold ([result '()])  ;; for* = nested loops
       ([j (in-range 1 elementos)]      ;
        [k (in-range 5 10)])
     (cons (+ (* k elementos) (+ k 1) ) result)
     (cons j result))))

; Crea un n^2 numero de cartas

(define (n2Cartas elementos)
 (reverse
  (for*/fold ([result '()])
             ([i (in-range 1 elementos)]
              [k (in-range 1 elementos)]
              [j (in-range 1 elementos)])
    (cons (modulo (+(* (* elementos (- k 1)) (+ elementos 2))(+ (* (- i 1) (- k 1))(- j 1))) elementos) result)
    (cons j result))))


;; Funcion de ejemplo de aleatoriedad.
(define m 2147483647)
(define a 1103515245)
(define c 12345)

(define randomFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)))

(randomFn 1)


(define (cardsSet elements numE maxC (randomFn 1)))
