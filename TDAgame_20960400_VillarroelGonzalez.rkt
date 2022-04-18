#lang racket

(provide game stackMode register)

;; Tipo de Funcion: Constructor
;; Descripcion: Funcion que almacenara los datos necesarios para el juego
;; Dominio: Entero, lista de listas, Funcion, funcion.
;; Recorrido: game (lista de listas)
;; Tipo de Recursion: No se hace uso de recursion
;;
(define (game numPlayers cardsSet mode randomFn) (list numPlayers cardsSet mode randomFn))

;; Tipo de Funcion: Selector
;; Descripcion: Funcion que consigue el numero de jugadores
;; Dominio: game
;; Recorrido: Entero
;; Tipo de Recursion: No se hace uso de recursion

(define (getNumPlayers game) (car game))

;; Tipo de Funcion: Selector
;; Descripcion: Funcion que consigue el mazo del juego
;; Dominio: game
;; Recorrido: Lista de listas
;; Tipo de Recursion: No se hace uso de recursion

(define (getMazo game) (cadr game))

;; Tipo de Funcion: Selector
;; Descripcion: Funcion que consigue el mode con el que se juega
;; Dominio: game
;; Recorrido: Funcion
;; Tipo de Recursion: No se hace uso de recursion

(define (getModo game) (caddr game))

;; Tipo de Funcion: Selector
;; Descripcion: Funcion que consigue la funcion de aleatorizacion
;; Dominio: game
;; Recorrido: funcion
;; Tipo de Recursion: No se hace uso de recursion

(define (getrandomFn game) (cadddr game))

;; Tipo de Funcion: Modificador
;; Descripcion: Agregara un usuario al juego
;; Dominio: String X game
;; Recorrido: game
;; Tipo de Recursion: No se hace uso de recursion

(define (register user juego)
  (cond
    [(empty? (cddddr juego)) (append juego (list (list user)))]
    [else
     (cond
       [(< (length (car(cddddr juego)))(car juego)) (append (game (getNumPlayers juego) (getMazo juego) (getModo juego) (getrandomFn juego))(list(flatten(append (cddddr juego) (list(list user))))))]
       [else (printf "Maximo de jugadores alcanzado\n") juego])]))


;; Tipo de Funcion: Selector
;; Descripcion: Mostrara las 2 cartas que esten en el tope del mazo
;; Dominio: cardsSet
;; Recorrido: cardsSet
;; Tipo de Recursion: No se hace uso de recursion

(define (stackMode mazo)
  (drop mazo (- (length mazo) 2)))
