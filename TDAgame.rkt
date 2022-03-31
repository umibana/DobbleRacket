#lang racket

;; (define (game))


(define (recursion n)
  (if  (= n 1)
       1
       (* n (recursion (- n 1)))))

(list 1 2 3)
