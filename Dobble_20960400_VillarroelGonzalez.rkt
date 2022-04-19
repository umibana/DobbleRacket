#lang racket

(require "TDAcardsSet_20960400_VillarroelGonzalez.rkt")
(require "TDAgame_20960400_VillarroelGonzalez.rkt")

(define m 2147483647)
(define a 1103515245)
(define c 12345)

(define randomFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)))

;; Ejemplo cardsSet
(cardsSet (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n") 3 6 randomFn)
(cardsSet 0 7 -1 randomFn)
(cardsSet 0 3 3 randomFn)

;; Ejemplo Dobble?
(dobble?(cardsSet (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n") 3 -1 randomFn))
(dobble?(cardsSet 0 7 30 randomFn))
(dobble?(list (list 1 2 3 4 5) (list 4 3 1 3 2)))

;; Ejemplo numCards
(numCards(cardsSet (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n") 3 -1 randomFn))
(numCards(cardsSet 0 7 30 randomFn))
(numCards(cardsSet 0 5 -1 randomFn))
;; Ejemplo nthCard
(nthCard(cardsSet (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n") 3 -1 randomFn) 1)
(nthCard(cardsSet 0 7 30 randomFn) 29)
(nthCard(cardsSet 0 5 -1 randomFn) 20)
;; Ejemplo findTotalCards
(findTotalCards(list "a" "b" "c" "d" "e" "j" "k" "l"))
(findTotalCards(list 1 2 3 4))
(findTotalCards(list 39 38 32 43 20))
;; Ejemplo requiredElements
(requiredElements(list "a" "b" "c" "d" "e" "j" "k" "l"))
(requiredElements(list 1 2 3 4))
(requiredElements(list 39 38 32 43 20))
;; Ejemplo missingCards
(missingCards(cardsSet 0 7 30 randomFn))
(missingCards(cardsSet 0 5 -1 randomFn))
(missingCards(list(list 1 2 3)(list 1 5 4)))
;; Ejemplo cardsSet->string
(cardsSet->string(cardsSet (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n") 3 -1 randomFn))
(cardsSet->string(cardsSet 0 7 30 randomFn))
(cardsSet->string(cardsSet 0 5 -1 randomFn))
;; TDA GAME
;; Ejemplo game
(game 4 (cardsSet (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n") 7 -1 randomFn)stackMode randomFn)
(game 2 (cardsSet 0 7 -1 randomFn)stackMode randomFn)
(game 3 (cardsSet 0 3 5 randomFn)stackMode randomFn)
;; Ejemplo stackMode
(stackMode (cardsSet (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n") 7 -1 randomFn))
(stackMode (cardsSet 0 7 -1 randomFn))
(stackMode (cardsSet 0 3 5 randomFn))
;; Ejemplo register

(register "DIINF"(game 4 (cardsSet (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n") 7 -1 randomFn)stackMode randomFn))
(register "USACH"(register "DIINF"(game 3 (cardsSet 0 3 5 randomFn)stackMode randomFn)))
(register "userDemas"(register "USACH"(register "DIINF"(game 2 (cardsSet 0 7 -1 randomFn)stackMode randomFn))))
