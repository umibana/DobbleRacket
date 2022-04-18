#lang racket

(define m 2147483647)
(define a 1103515245)
(define c 12345)

(define randomFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)))

(provide game stackMode)

(define (game numPlayers cardsSet mode randomFn) (list numPlayers cardsSet mode randomFn))

(define (getNumPlayers game) (car game))
(define (getMazo game) (cadr game))
(define (getModo game) (caddr game))
(define (getrandomFn game) (cadddr game))

(define (register user juego)
  (cond
    [(empty? (cddddr juego)) (append juego (list (list user)))]
    [else
      (cond
        [(< (length (car(cddddr juego)))(car juego)) (append (game (getNumPlayers juego) (getMazo juego) (getModo juego) (getrandomFn juego))(list(flatten(append (cddddr juego) (list(list user))))))]
        [else (printf "Maximo de jugadores alcanzado\n") juego])]))



(define (stackMode mazo)
  (drop mazo (- (length mazo) 2)))
