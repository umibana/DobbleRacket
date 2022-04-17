#lang racket

(provide game stackMode)

(define (game numPlayers cardsSet mode)
  (cond
    [(eq? mode stackMode) "Hello"]
    [else '()]))

(define (getPlayers game) (car game))
(define (register game)
  (cond
    [(< )(car numPlyaer)]))


(define (stackMode mazo)
  (drop mazo (- (length mazo) 2)))
