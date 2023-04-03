#lang racket
(require racket/list)
(require data/spmatrix)
(require racket/system)
(require "treeNario.rkt")

;crea los valores necesarios para implementar el minimax, en este caso es improbable tener una jugada mejor o superior a estos valores alpha beta
;crea el nodoTemp el cual es un nodo vacio con la lista de la primer generacion de nodos del arbol
(define (minMaxTree tree)
  (define nodoTemp (makeNodoVoid tree))
  (define alpha (makeNodoVoid3 -200))
  (define beta (makeNodoVoid3 200))
  (minMax-Poda nodoTemp 0 alpha beta))

;Algoritmo miniMax, verifica si el nodo actual es una rama, de ser así devuelve el nodo, sino dependiendo de la profundidad que vaya
;busca la parte min o max del nodo, iniciamos con profundidad xero porque buscamos maximizar la jugada en profundidad 0
(define (minMax-Poda nodo profundidad alpha beta)
  (if (empty? (listaNodo nodo))
      nodo
      (if (= (remainder profundidad 2) 0)
          (maxPart1 nodo profundidad alpha beta)
          (minPart1 nodo profundidad alpha beta))))

;Parte max del algoritmo minimax, corta el subarbor de forma que este esté devolviendo un nodo más pequeño que el que recibió, osea lo poda
(define (maxPart1 nodo profundidad alpha beta)
  (define mejorValor (makeNodoVoid2)) ;mejor valor
  (define lista (listaNodo nodo))
  (for ([hijo (in-list lista)])
    (define valor (minMax-Poda hijo (+ profundidad 1) alpha beta))
    (set! mejorValor (max alpha valor))
    (set! alpha (max alpha mejorValor))
    (when (<= (puntuacionNodo beta) (puntuacionNodo alpha)) (set! lista '())))
  mejorValor)

;Parte min del algoritmo minimax, corta el subarbor de forma que este esté devolviendo un nodo más grande que el que recibió, osea lo poda
(define (minPart1 nodo profundidad alpha beta)
  (define mejorValor (makeNodoVoid2)) ;mejor valor
  (define lista (listaNodo nodo))
  (for ([hijo (in-list lista)])
    (define valor (minMax-Poda hijo (+ profundidad 1) alpha beta))
    (set! mejorValor (min beta valor))
    (set! beta (min beta mejorValor))
    (when (>= (puntuacionNodo beta) (puntuacionNodo alpha)) (set! lista '())))
  mejorValor)

;Funcion que se encarga de generar el valor minimo entre 2 variables
(define (min val1 val2)
        (if (>= (puntuacionNodo val1) (puntuacionNodo val2))
            val2
            val1))

;Funcion que se encarga de generar el valor maximo entre 2 variables
(define (max val1 val2)
        (if (<= (puntuacionNodo val1) (puntuacionNodo val2))
            val2
            val1))

(provide minMaxTree)


