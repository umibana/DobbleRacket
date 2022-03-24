#lang racket

;; Funcion de seleccion aleatoria, dejada de ejemplo por pauta.
(define m 2147483647)
(define a 1103515245)
(define c 12345)

(define randomFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)))
