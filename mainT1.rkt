#lang racket (require racket/list)
(require data/spmatrix)
(require racket/system)
(require "treeNario.rkt")
(require "eval.rkt")
(require "minMax_Poda.rkt")

(define matrizGame (build-matrix 5 6 (lambda (i j) 0))) ;'(mlist  (mlist 0 0 0 0 0 0) (mlist 0 0 0 0 0 0) (mlist 0 0 0 0 0 0) (mlist 0 0 0 0 0 0) (mlist 0 0 0 0 0 0)))
;######################################################
;################### Menus

(define (mostrar-menu )
  (displayln "Selecciona una opción:")
  (displayln "1. Jugar colocando automaticamente las fichas")
  (displayln "2. Jugar colocando manualmente las fichas (Deshabilitada)")
  (displayln "3. Salir")
  (display "Opción: ")
  (read))

(define (opcion1)
  (displayln "Has seleccionado la opción 1.")
  (matriz-auto)
  (play (gna 1 2))
  )

(define (opcion2)
  (matriz-auto)
  (displayln "Has seleccionado la opción 2.")
  (draw-board)
  ;(makeTree matrizGame (gna 1 2))
  ;(display (makeTree matrizGame (gna 1 2)))
  (display-matrix matrizGame)
  ;(printTree (makeTree matrizGame 1))
  (display( minMaxTree (makeTree matrizGame 1))
  ))

(define (opcion3)
  (displayln "Has seleccionado la opción 3.")
  (exit 0))



;#####################################################
;#################  BOARD  ###########################

;Función que crea el tablero inicial, utilizando la matriz global
;Sin entradas y la salida es un string del tablero
(define (draw-board)
  (display
"          TABLERO
      ****************
      ****1 2 3 4 5 6*
      ****************
 ")
  (display "     *1* ")
  (create-row (matrix-ref matrizGame 0 0)) (create-row (matrix-ref matrizGame 0 1)) (create-row (matrix-ref matrizGame 0 2))
  (create-row (matrix-ref matrizGame 0 3)) (create-row (matrix-ref matrizGame 0 4)) (create-row (matrix-ref matrizGame 0 5))
  (display "\n      *2* ") (create-row (matrix-ref matrizGame 1 0)) (create-row (matrix-ref matrizGame 1 1)) (create-row (matrix-ref matrizGame 1 2))
  (create-row (matrix-ref matrizGame 1 3)) (create-row (matrix-ref matrizGame 1 4)) (create-row (matrix-ref matrizGame 1 5)) (display "\n      *3* ")
  (create-row (matrix-ref matrizGame 2 0)) (create-row (matrix-ref matrizGame 2 1)) (create-row (matrix-ref matrizGame 2 2))
  (create-row (matrix-ref matrizGame 2 3)) (create-row (matrix-ref matrizGame 2 4)) (create-row (matrix-ref matrizGame 2 5)) (display "\n      *4* ")
  (create-row (matrix-ref matrizGame 3 0)) (create-row (matrix-ref matrizGame 3 1)) (create-row (matrix-ref matrizGame 3 2))
  (create-row (matrix-ref matrizGame 3 3)) (create-row (matrix-ref matrizGame 3 4)) (create-row (matrix-ref matrizGame 3 5)) (display "\n      *5* ")
  (create-row (matrix-ref matrizGame 4 0)) (create-row (matrix-ref matrizGame 4 1)) (create-row (matrix-ref matrizGame 4 2))
  (create-row (matrix-ref matrizGame 4 3)) (create-row (matrix-ref matrizGame 4 4)) (create-row (matrix-ref matrizGame 4 5)) (display "\n\n ")
   )

;imprime y crea una posición del tablero
(define (create-row num)
  (display (create-pos num))
  )

;Función encargada de devolver un string dependiendo de lo que entre si es cero un cuadro vacio sino un numero
(define (create-pos num)
  (if (equal? num 0) (integer->char #x25EF)
      
      (if (equal? num 1)
          (integer->char #x2460)
          (integer->char #x2461)
          );(string-append " "(number->string num))
      ))

;#####################################################
;############### Generado automatico

(define (matriz-auto)
  (localize-pos (gna 0 5) (gna 0 6) 1) ;#1
  (localize-pos (gna 0 5) (gna 0 6) 1)
  (localize-pos (gna 0 5) (gna 0 6) 1)
  (localize-pos (gna 0 5) (gna 0 6) 1)
  (localize-pos (gna 0 5) (gna 0 6) 1)
  (localize-pos (gna 0 5) (gna 0 6) 1)
  (localize-pos (gna 0 5) (gna 0 6) 1)
  (localize-pos (gna 0 5) (gna 0 6) 1)
  (localize-pos (gna 0 5) (gna 0 6) 1)
  (localize-pos (gna 0 5) (gna 0 6) 1)
  (localize-pos (gna 0 5) (gna 0 6) 1)
  (localize-pos (gna 0 5) (gna 0 6) 1) ;#12 esto es el numero de fichas del jugador "n", son 12 fichas por cada jugador
  
  
  (localize-pos (gna 0 5) (gna 0 6) 2)
  (localize-pos (gna 0 5) (gna 0 6) 2)
  (localize-pos (gna 0 5) (gna 0 6) 2)
  (localize-pos (gna 0 5) (gna 0 6) 2)
  (localize-pos (gna 0 5) (gna 0 6) 2)
  (localize-pos (gna 0 5) (gna 0 6) 2)
  (localize-pos (gna 0 5) (gna 0 6) 2)
  (localize-pos (gna 0 5) (gna 0 6) 2)
  (localize-pos (gna 0 5) (gna 0 6) 2)
  (localize-pos (gna 0 5) (gna 0 6) 2)
  (localize-pos (gna 0 5) (gna 0 6) 2)
  (localize-pos (gna 0 5) (gna 0 6) 2)
  )

;pregunta si la posición es cero, si pregunta si es valida la posición si lo es cambia la posición por el numero sino, continua seleccionando aleatoriamente hasta que sea posible
(define (localize-pos row col num)
  (if (equal? (matrix-ref matrizGame row col) 0)
      (if (validate-pos matrizGame row col num)
          (matrix-set! matrizGame row col num)
          (localize-pos (gna 0 5) (gna 0 6) num)
          )
      (localize-pos (gna 0 5) (gna 0 6) num)
  )
)
  





;#####################################################
;############### Otros y main

(define (amountStamps player row col count)
  (if (equal? row 4)
      (if (equal? col 5)
          (if (equal? player (matrix-ref matrizGame row col))
              (+ count 1)
              count
              )
          (if (equal? player (matrix-ref matrizGame row col))
              (amountStamps player row (+ col 1) (+ count 1))
              (amountStamps player row (+ col 1) count)
          )
      )
      (if (equal? col 5)
          (if (equal? player (matrix-ref matrizGame row col))
              (amountStamps player (+ row 1) 0 (+ count 1))
              (amountStamps player (+ row 1) 0 count)
              )
          (if (equal? player (matrix-ref matrizGame row col))
              (amountStamps player row (+ col 1) (+ count 1))
              (amountStamps player row (+ col 1) count)
          )
      )
    )
  )

;####### Generar numeros aleatorios entre minimo y el maximo
;sin entradas y sale 1 numero del minimo al maximo
(define (gna min max) ;generar numero aleatorio gna
  (inexact->exact (+ min (floor (* max (random)))))
  )

;Funcion main del programa
(define (main)
  (let loop ()
    (let ((opcion (mostrar-menu)))
      (cond ((= opcion 1) (opcion1))
            ((= opcion 2) (opcion2))
            ((= opcion 3) (opcion3))
            (else (displayln "Opción inválida. Intenta de nuevo.")))
      (loop))))

;#####################################################
;################ Jugabilidad

(define (play currentPlayer)
  (if (equal? (verifyWinner) 0)
      (selectMove1 currentPlayer)
      (displayWinner)
  )
)

(define (displayWinner)
  (displayln (string-append "Felicidades, el ganador del juego es el jugador "(number->string (verifyWinner))))
  (exit 0)
  )

(define (verifyWinner)
  (if (<= (amountStamps 1 0 0 0) 2) ;si la cantidad de fichas del jugador 1 es 2 o menos
      2                              ;gana el jugador 2
      (if (<= (amountStamps 2 0 0 0) 2) ;sino, si el jugador 2 tiene menos de 3 fichas
          1                          ;gana el jugador 1
          0                          ;sino no hay ganador
      )
  )
)

;############################################
;#################### Pedir movimientos

;Funcion que pide el numero del primer digito en este caso la fila de la ficha que se quiere mover
(define (selectMove1 player)
    (draw-board)
    (let loop ()
    (let ((opcion (selectMoveOnBoardRow player)))
      (cond ((= opcion 1) (selectMove12 0 player))
            ((= opcion 2) (selectMove12 1 player))
            ((= opcion 3) (selectMove12 2 player))
            ((= opcion 4) (selectMove12 3 player))
            ((= opcion 5) (selectMove12 4 player))
            (else (displayln "Opción inválida. Intenta de nuevo.")))
      (loop)))
  )

;Funcion que recibe la fila de la ficha deseada
(define (selectMoveOnBoardRow player)
  (displayln (string-append "Turno del jugador numero " (number->string player)))
  (displayln "Selecciona la Fila de sus fichas que quiera y pueda mover: ")
  (read))

;Funcion que recibe la columna deseada
(define (selectMoveOnBoardCol player)
  (displayln "Ahora selecciona la columna de su fichas que quiera y pueda mover: ")
  (read))

;funcion que pide el numero de la posición de la columna y al obtenerla envía a verificar la ficha si es posible moverla 
(define (selectMove12 row player)
  (let loop ()
    (let ((opcion (selectMoveOnBoardCol player)))
      (cond ((= opcion 1) ( validateStampPosPlayer row 0 player))
            ((= opcion 2) ( validateStampPosPlayer row 1 player))
            ((= opcion 3) ( validateStampPosPlayer row 2 player))
            ((= opcion 4) ( validateStampPosPlayer row 3 player))
            ((= opcion 5) ( validateStampPosPlayer row 4 player))
            ((= opcion 6) ( validateStampPosPlayer row 5 player))
            (else (displayln "Opción inválida. Intenta de nuevo.")))
      (loop))))

;Valida si la ficha pertenece al jugador y valida si es posible moverla
(define (validateStampPosPlayer row col player)
  (if (equal? (matrix-ref matrizGame row col) player)
      (verifyMove row col player)
      (noValidPos row col player)))

;Funcion que imprime que una posición no es valida, de ser así pide otra posición a mover
(define (noValidPos row col player)
  (displayln (string-append "No es valida la posición " (number->string (+ row 1)) "," (number->string (+ col 1))))
  (selectMove1 player)
  )

;Funcion que imprime que una posición no es valida, de ser así pide otra posición a mover
(define (noValidPosVoid row col player)
  (displayln (string-append "No es valida la posición " (number->string (+ row 1)) "," (number->string (+ col 1)) " debido a que no hay donde moverse"))
  (selectMove1 player)
  )

(define (verifyMove row col player)
  (if (canMove matrizGame row col)
      (directionMove row col player)       
      (noValidPosVoid row col player)
      )
 )

;#############################################
;################### Pide hacia donde mover la ficha y movimientos

;menu de pedir hacia donde mover
(define (directionMove row col player)
  (let loop ()
    (let ((opcion (getDirection)))
      (cond ((= opcion 1) ( validateDirectionPosible row col player 1))
            ((= opcion 2) ( validateDirectionPosible row col player 2))
            ((= opcion 3) ( validateDirectionPosible row col player 3))
            ((= opcion 4) ( validateDirectionPosible row col player 4))
            (else (displayln "Opción inválida. Intenta de nuevo.")))
      (loop)))
)

;Funcion que espera la dirección en la que el jugador vaya a mover la ficha
(define (getDirection)
  (displayln "1. Arriba")(displayln "2. Abajo")(displayln "3. Izquierda")(displayln "4. Derecha")
  (displayln "Selecciona seleccione hacia donde quiere mover su ficha: ")
  (read))

;Funcion encargada de validar si es posible mover hacia cierta dirección, de ser así manda a mover la ficha
(define (validateDirectionPosible row col player direction)
  (if (equal? direction 1)
      (if (topVoid matrizGame row col)
          (moveStamp (- row 1) col player row col)
          ((display "La ficha no puede moverse hacia arriba, seleccione otra posición")(directionMove row col player))
          )
      (if (equal? direction 2)
          (if (botVoid matrizGame row col)
              (moveStamp (+ row 1) col player row col)
              ((display "La ficha no puede moverse hacia abajo, seleccione otra posición")(directionMove row col player))
          )
          (if (equal? direction 3)
               (if (leftVoid matrizGame row col)
                   (moveStamp row (- col 1) player row col)
                   ((display "La ficha no puede moverse hacia la izquierda, seleccione otra posición")(directionMove row col player))
                   )
               (if (equal? direction 4)
                   (if (rightVoid matrizGame row col)
                       (moveStamp row (+ col 1) player row col)
                       ((display "La ficha no puede moverse hacia la derecha, seleccione otra posición")(directionMove row col player))
                       )
                   ((display "La ficha no se puede mover a ningun sitio, seleccione otra ficha")(selectMove1 player))
                   )
          )
      )
  )
)

;Funcion encargada de mover una ficha a la posición row col y quitarla de la posición replaceRow replaceCol, seguidamente pasa a validar si genera trios
(define (moveStamp row col player replacRow replaceCol)
  (matrix-set! matrizGame row col player)
  (matrix-set! matrizGame replacRow replaceCol 0)
  (validateMoveStamp row col player))                        ; <<<<<<<<<<<<<<<<------------validar si hay trio, si hay pedir que ficha quitar, si no hay cambiar turno

;Funcion valida si una posición tiene tríos en alguna dirección, si tiene manda a pedir una row col de ficha a quitar, sino pasa turno
(define (validateMoveStamp row col player)
  (if (validate-pos matrizGame row col player)
      (if (equal? player 1)
          (play 2)
          (play 1))
      (removeStampPlayer player)
  )
)

;#############################################
;################### Pedir ficha del contrario que quitar

;espera la opción del usuario, menu para quitar una ficha del contrario
(define (removeStampPlayer player)
  (draw-board)
    (let loop ()
    (let ((opcion (selectRemoveOnBoardRow player)))
      (cond ((= opcion 1) (selectRemove12 0 player))
            ((= opcion 2) (selectRemove12 1 player))
            ((= opcion 3) (selectRemove12 2 player))
            ((= opcion 4) (selectRemove12 3 player))
            ((= opcion 5) (selectRemove12 4 player))
            (else (displayln "Opción inválida. Intenta de nuevo.")))
      (loop)))
)

;Funcion que recibe la fila de la ficha que se deseaq quitar de la ficha del jugador contrario
(define (selectRemoveOnBoardRow player)
  (displayln (string-append "Se a generado un trío, es posible remover una ficha del contrincante.\nTurno del jugador numero " (number->string player)))
  (displayln "Selecciona la Fila de la fichas que quiere quitar del contrincante: ")
  (read))

(define (selectRemove12 row player)
  (let loop ()
    (let ((opcion (selectRemoveOnBoardCol player)))
      (cond ((= opcion 1) ( validateStampPosPlayerOpponent row 0 player))
            ((= opcion 2) ( validateStampPosPlayerOpponent row 1 player))
            ((= opcion 3) ( validateStampPosPlayerOpponent row 2 player))
            ((= opcion 4) ( validateStampPosPlayerOpponent row 3 player))
            ((= opcion 5) ( validateStampPosPlayerOpponent row 4 player))
            ((= opcion 6) ( validateStampPosPlayerOpponent row 5 player))
            (else (displayln "Opción inválida. Intenta de nuevo.")))
      (loop))))

;Funcion que recibe la columna deseada de la ficha que se desea quitar
(define (selectRemoveOnBoardCol player)
  (displayln "Ahora selecciona la columna de la fichas que quiera remover del contrincante: ")
  (read))

;Valida si la ficha pertenece al jugador y valida si es posible moverla
(define (validateStampPosPlayerOpponent row col player)
  (if (equal? player 1)
      (if (equal? (matrix-ref matrizGame row col) 2)
          (removePos row col 2)
          (noValidPos2 row col player))
      (if (equal? (matrix-ref matrizGame row col) 1)
          (removePos row col 1)
          (noValidPos2 row col player))
      )
  )

(define (removePos row col player)
  (matrix-set! matrizGame row col 0)
  (play player)
  )

;Funcion que imprime que una posición no es valida, de ser así pide otra posición a mover
(define (noValidPos2 row col player)
  (displayln (string-append "No es valida la posición " (number->string (+ row 1)) "," (number->string (+ col 1))))
  (removeStampPlayer player)
  )

(main)
