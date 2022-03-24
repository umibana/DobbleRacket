
;; Para la creacion del set de cartas se hara uso del algoritmo
;; proporcionado por Micky Dore [https://mickydore.medium.com/the-dobble-algorithm-b9c9018afc52]
;;
;;
(define elementCards 4)

;; (define (cardsSet elementos n_elementos max_cartas randomFn)
;;   (for ([i (in-range 1 elementos)])
;;     (cons i '(1))))

(define (primeraCarta elementos)
  (for/fold ([result '()])
            ([i (in-range 1 elementos)])
    (cons i result)))







    



(primeraCarta elementCards)
