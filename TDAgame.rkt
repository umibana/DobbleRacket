#lang racket

(provide game stackMode)
;; (define (game numPlayers cardsSet mode))


(define (stackMode mazo)
  (drop mazo (- (length mazo) 2)))
