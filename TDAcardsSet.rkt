
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
   (for*/fold ([result '(1)])
       ([i (in-range 1 elementos)]
        [j (in-range 1 elementos)])
     (cons j result)
     (cons i result))))


(nCartas elementCards)













