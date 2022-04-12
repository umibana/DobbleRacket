;; Para la creacion del set de cartas se haa uso del algoritmo
;; proporcionado por Micky Dore [https://mickydore.medium.com/the-dobble-algorithm-b9c9018afc52]
;;
;; El algoritmo hace uso for loops. Para la implementacion en Racket se usara recursion
#lang racket

(provide cardsSet cardsSet->string)
;;Funcion de ejemplo de aleatoriedad.
(define m 2147483647)
(define a 1103515245)
(define c 12345)

(define randomFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)))


;; Primera parte del algoritmo (creacion de primera carta)
(define (primeraCarta n)
  (cond
    [(= n 0) (list 1)]
    [(> n 0) (cons (+ n 1) (primeraCarta (- n 1)))]))

; Segunda parte del algoritmo (Creacion de n Cartas)
; Se crea una funcion nCartas y nCartas_j (Auxiliar)
; La funcion nCartas_j sera llamada por la funcion principal (nCartas) recursivamente
; Y generara los simbolos para cada carta
; nCartas, en cambio generara la carta y recursivamente llamando a nCartas_j, generara los simbolos

; Defino la operacion del algoritmo para no colocarlo dentro de la funcion
(define nAlgoritmo
  (lambda (n i j)(+ (* n i ) (+ j 1))))


(define (nCartas_j n i j)
  (cond
    [(and (= i 1)(= j 1)) (list (nAlgoritmo n i j))]
    [(= j 0) (list 1)]
    [(> j 0) (cons (nAlgoritmo n i j) (nCartas_j n i (- j 1)))]))


(define (nCartas n i j)
  (cond
    [(= i 1) (cons 1 (nCartas_j n i n))]
    [(> i 1) (cons (nCartas n (- i 1) j) (nCartas_j n i n))]))

; Tercera parte del algoritmo
; Define la operacion del algoritmo para que no quede tan largo dentro de la funcion
(define n2Algoritmo
  (lambda (n i j k) (+(+ (* n (- k 1)) (+ n 2))(modulo (+ (* (- i 1) (- k 1))(- j 1) )n))))
; loop k
(define (n2Cartas_k n i j k)
  (cond
    [(= k 1) (cons (n2Algoritmo n i j k) (list (+ i 1)))]
    [(> k 1) (cons (n2Algoritmo n i j k ) (n2Cartas_k n i j (- k 1)))]))
; loop j
(define (n2Cartas_j n i j k)
  (cond
    [(= j 1) (n2Cartas_k n i j n)]
    [(> j 1) (cons (n2Cartas_j n i (- j 1) k) (n2Cartas_k n i j k))]))
; loop i
(define (n2Cartas n i j k)
  (cond
    [(= i 1) (n2Cartas_j n i n n)]
    [(> i 1) (cons (n2Cartas n (- i 1) n n) (n2Cartas_j n i n n))]))


(define aplanarMazo
  (lambda (ordenPlano)
    (append(flatten(reverse(primeraCarta ordenPlano)))
           (append(flatten(nCartas ordenPlano ordenPlano ordenPlano))
                  (flatten(n2Cartas ordenPlano ordenPlano ordenPlano ordenPlano))))))


;; Parte de cardsSet->String
;; Recursion natural
(define (ordenarEn lst n)
  (cond
    [(not (empty? lst)) (cons (take lst n) (ordenarEn (drop lst n) n))]
    [else '()]))

(define ordenarMazo (lambda (ordenPlano)
                      (ordenarEn (aplanarMazo ordenPlano) (+ ordenPlano 1))))

(define (remplazarCartas buscar remplazo carta)
  (cond
    [(null? carta) '()]
    [(list? (car carta))
     (cons (remplazarCartas buscar remplazo (cdr carta))
           (remplazarCartas buscar remplazo (cdr carta)))]
    [(equal? buscar (car carta))
     (cons remplazo
           (remplazarCartas buscar remplazo (cdr carta)))]
    [else (cons (car carta)
                (remplazarCartas buscar remplazo (cdr carta)))]))


(define remplazarMazo
  (lambda (simbolos mazo [i 0])
     (cond
       [(= (+ i 1) (length simbolos))
        (remplazarCartas (+ i 1) (list-ref simbolos i) mazo)]
       [else (remplazarMazo simbolos
                            (remplazarCartas (+ i 1) (list-ref simbolos i) mazo)
                            (+ i 1))])))


(define armarMazo
  (lambda (elementos)
    (cond
      [(integer? elementos) (ordenarMazo elementos)]
      [else (error "error: ingrese elementos validos")])))


(define (cardsSet [elementos 0] [numE 8] [maxC 57])
  (cond
    [(list? elementos)
     (cond
        [(or (> maxC (+(* numE (- numE 1))))(negative? maxC))
         (ordenarEn (remplazarMazo elementos (aplanarMazo (- numE 1))) numE)]
        [else
         (take(ordenarEn (remplazarMazo elementos (aplanarMazo (- numE 1))) numE)maxC)])]
    [else
      (cond
        [(or (> maxC (+(* numE (- numE 1))))(negative? maxC))
         (armarMazo (- numE 1))]
        [else (take (armarMazo (- numE 1))maxC)])]))


(define (dobble? mazo)
  (cond
    [(and
      (= (numCards mazo) (+ 1 (*(length (car mazo)) (- (length (car mazo)) 1))))
      (!=))]))


(define (cardsSet->string mazo [i 1])
  (cond
    [(null? mazo) (printf "Mazo Actual")]
    [else (printf "Carta ~a: ~s\n" i (car mazo))
          (cardsSet->string (cdr mazo ) (+ i 1))])) ;

(define (numCards mazo) (length mazo))
(define (nthCard mazo nth) (list-ref mazo nth))

(define (findTotalCards carta)
  (printf "Se necesitan ~a cartas para un mazo válido"
     (numCards
      (cardsSet 0 (length carta)))))

(define (requiredElements carta)
  (printf "Se necesitaran ~a elementos en total para un conjunto válido"
    (length
      (cardsSet 0 (length carta)))))

;; Falta implementar correctamente
(define (missingCards mazo)
     (remove mazo (cardsSet mazo (length mazo))))








;;(define (dobble? n)
