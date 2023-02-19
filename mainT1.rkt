#lang racket

(define matrizGame  '((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)))
;######################################################
;################### Menus

(define (mostrar-menu )
  (displayln "Selecciona una opción:")
  (displayln "1. Jugar colocando automaticamente las fichas")
  (displayln "2. Jugar colocando manualmente las fichas")
  (displayln "3. Salir")
  (display "Opción: ")
  (read))

(define (opcion1)
  (displayln "Has seleccionado la opción 1.")
  (draw-board))

(define (opcion2)
  (generar-numero-aleatorio))

(define (opcion3)
  (displayln "Has seleccionado la opción 3."))

;#####################################################
;#################  BOARD  ###########################

;Función que crea el tablero inicial, utilizando la matriz global
;Sin entradas y la salida es un string del tablero
(define (draw-board)
  (display
"
   A B C D E F
 ")
  (display "1")
  (create-row (list-ref matrizGame 0))
  (display "\n 2")
  (create-row (list-ref matrizGame 1))
  (display "\n 3")
  (create-row (list-ref matrizGame 2))
  (display "\n 4")
  (create-row (list-ref matrizGame 3))
  (display "\n 5")
  (create-row (list-ref matrizGame 4))
  (display "\n ") 
          )

;# funcion encargada de crear un string de una fila entrante dependiendo de lo que esta reciba
;recibe una lista de 6 numeros sale un string
(define (create-row row)
  (display (string-append (create-pos(first row)) (create-pos (second row)) (create-pos (third row)) (create-pos (fourth row)) (create-pos (fifth row)) (create-pos (sixth row))))
  )

;Función encargada de devolver un string dependiendo de lo que entre si es cero un cuadro vacio sino un numero
(define (create-pos num)
  (if (equal? num 0) " □" num
  ))

;#####################################################
;############### Otros y main

;####### Generar numeros aleatorios entre 1 y 2
;sin entradas y sale 1 numero del 1 al 2
(define (generar-numero-aleatorio)
  (+ 1 (floor (* 2 (random)))))

;Funcion main del programa
(define (main)
  (let loop ()
    (let ((opcion (mostrar-menu)))
      (cond ((= opcion 1) (opcion1))
            ((= opcion 2) (opcion2))
            ((= opcion 3) (opcion3))
            (else (displayln "Opción inválida. Intenta de nuevo.")))
      (loop))))

;(main)
