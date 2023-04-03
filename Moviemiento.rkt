#lang racket/gui
(require racket/list)
(require data/spmatrix)
(require racket/system)
(require racket/os)
(require racket/date)
(require "treeNario.rkt")
(require "eval.rkt")
(require "minMax_Poda.rkt")
(require (lib "graphics.ss"  "graphics"))
(require racket/random)
(open-graphics) 

(define matrizGame (build-matrix 5 6 (lambda (i j) 0)))

;############### Generado automatico
(define jugador 1)
(define IA 0)

;Función que genera la matriz de forma automática
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
  (localize-pos (gna 0 5) (gna 0 6) 2))

;pregunta si la posición es cero, si pregunta si es valida la posición si lo es cambia la posición por el numero sino, continua seleccionando aleatoriamente hasta que sea posible
(define (localize-pos row col num)
  (if (equal? (matrix-ref matrizGame row col) 0)
      (if (validate-pos matrizGame row col num)
          (matrix-set! matrizGame row col num)
          (localize-pos (gna 0 5) (gna 0 6) num))
      (localize-pos (gna 0 5) (gna 0 6) num)))

;####### Generar numeros aleatorios entre minimo y el maximo
;sin entradas y sale 1 numero del minimo al maximo
(define (gna min max) ;generar numero aleatorio gna
  (inexact->exact (+ min (floor (* max (random))))))

;retorna la matriz del juego
(define (return-matrix)
  matrizGame)

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
  (create-row (matrix-ref matrizGame 4 3)) (create-row (matrix-ref matrizGame 4 4)) (create-row (matrix-ref matrizGame 4 5)) (display "\n\n "))

;imprime y crea una posición del tablero
(define (create-row num)
  (display (create-pos num)))

;Función encargada de devolver un string dependiendo de lo que entre si es cero un cuadro vacio sino un numero
(define (create-pos num)
  (if (equal? num 0) (integer->char #x25EF)
      
      (if (equal? num 1)
          (integer->char #x2460)
          (integer->char #x2461)
          )));(string-append " "(number->string num))
      

;partes de la GUI
(define matriz return-matrix)
(define z (open-viewport "Dara"  500 500)); s
(define p (open-pixmap "Dara" 550 500)) ; 5
(define margen 10) ;se deje un margen de 18 pix
(define x 2) ;5e define una variable x donde se
(define y 6) ;se define una variable y donde se
(define x2 0) ; se define una variable x2 donde
(define y2 0) ; se define una variable y2 donde
(define negro "black") ; se define una variable
(define gris (make-rgb 0.40 0.40 0.40)) ; se de
(define grisA "") ; se define una variable gris
(define blancoA "") ; se define une variable bl
(define PS "Posicién Actual") ; se define una v
(define MN  "Movimiento no valido") ; se define
(define FT "Fuera del Tablero")
(define u 0)
(define h 0)
(define v 0)
(for ([v (in-range 11 400 80)]) ; V es le variable en el eje y
  (if (= u 0)
      (for ([h (in-range 11 480 160)])
        ((draw-solid-rectangle p) (make-posn h v) 79 79 "white")
        (set! u (+ 1 1))
        )
      (for ([h (in-range 91 651 160)])
        ((draw-solid-rectangle p) (make-posn h v) 79 79 "white")
      (set! u (- 1 1))
        )
      )
  )
;verticales
(for ([h (in-range 10 500 80)])
  ((draw-line p) (make-posn h 10) (make-posn h 410) "black")
  )
;horizontales
(for ([v (in-range 10 490 80)])
((draw-line p) (make-posn 10 v) (make-posn 490 v) "black"))

;dibujar
(define (ficha i j color)
  (cond
    ((and (and (> i -1) (<= i 7)) (and (> j -1) (< j 8)))
          (begin
            (define a (+ (* i 80) margen))
            (define b (+ (* j 80) margen))
            ((draw-pixmap p) "ficha1.png"(make-posn (+ a 22) (+ b 15)))
            (copy-viewport p z)
             ((draw-solid-rectangle p ) (make-posn (+ a 22) (+ b 15)) 0 0 "white")))))

(define (ficha2 i j color)
  (cond
    ((and (and (> i -1) (<= i 7)) (and (> j -1) (< j 8)))
          (begin
            (define a (+ (* i 80) margen))
            (define b (+ (* j 80) margen))
            ((draw-pixmap p) "ficha2.png"(make-posn (+ a 22) (+ b 15)))
            (copy-viewport p z)
             ((draw-solid-rectangle p ) (make-posn (+ a 22) (+ b 15)) 0 0 "white")))))

;encuentra la coordenada de la celda 
(define (encontrarCordenanda)
  (for ([i (in-range 0 5)]) ;5
    (for ([j (in-range 0 6)]) ;6
      (displayln (matrix-ref matrizGame i j))
      (if (equal? (matrix-ref matrizGame i j) 1)
          (ficha j i negro)
          (if(equal? (matrix-ref matrizGame i j) 2)
              (ficha2 j i negro)
              #f)))))


(define (pos n)
  (truncate (/ (- n margen) 80)))

;Funcion que se encarga de verificar si una posición puede moverse hacia alguna dirección, de ser así significa que no es una ficha encerrada
(define (verificarMovimiento x y x2 y2 )
  (if (< (giveMove x y x2 y2) 5 )
      (verificarMovimientoValido x y x2 y2 (giveMove x y x2 y2) );stamp
      (waitPosition x y)))

;verifica que la ficha se pueda mover hacia una dirección específica
(define (verificarMovimientoValido x y x2 y2 dir)
  (if(canMoveDir matrizGame x y dir)
      (moveStamp x y x2 y2 )
      (waitPosition x y)))

;se encarga de ,odificar la matriz segun un movimiento y enviar a modificar la GUI 
(define (moveStamp row col rowMove colMove)
  (matrix-set! matrizGame row col 0)
  (matrix-set! matrizGame rowMove colMove jugador)
  (moverFichaTablero row col rowMove colMove )
  (validateMoveStamp rowMove colMove))

;verifica si el mvimiento generó un trío, si se generó envia al usuario a remover una ficha del contrario, sino pasa de turno
(define (validateMoveStamp row col)
  (if (validate-pos matrizGame row col jugador)
      (if (equal? jugador 1)
          (set! jugador 2)
          (set! jugador 1))
      (removeStampPlayer)))

;espera el click del usuario para que remueva una ficha, si es la IA envia a remover una ficha segun el miniMax
(define (removeStampPlayer)
  (if (equal? jugador IA)
      (removeIA)
       (if (equal? (left-mouse-click? (get-mouse-click z)) #f)
       (removeStampPlayer)
       (begin
        (set! x2 (pos (posn-x (query-mouse-posn z)))) ;y
        (set! y2 (pos (posn-y (query-mouse-posn z)))) ;x
        (cond
          ((and (equal? x x2) (equal? y y2)) (removeStampPlayer))
          ((or (or (>(posn-x(query-mouse-posn z))485)(<(posn-x(query-mouse-posn z))10))(or(>(posn-y(query-mouse-posn z))405)(<(posn-y(query-mouse-posn z))10)))(juego))
          (else (validateStampPosPlayerOpponent y2 x2)))))))

;Valida si la ficha pertenece al jugador y valida si es posible removerla
(define (validateStampPosPlayerOpponent row col)
  (if (equal? jugador 1)
      (if (equal? (matrix-ref matrizGame row col) 2)
          (removePos row col)
          (removeStampPlayer))
      (if (equal? (matrix-ref matrizGame row col) 1)
          (removePos row col)
          (removeStampPlayer))))

;remueve una ficha en la GUI
(define (removePos row col)
  (define a (+ (* row 80) margen))
  (define b (+ (* col 80) margen))
  (matrix-set! matrizGame row col 0)
  ((draw-solid-rectangle p ) (make-posn (+ b 12) (+ a 5)) 60 60 "white")
  (copy-viewport p z)
  (display (verifyWinner))
  (if (equal? (verifyWinner) 0)
      (if (equal? jugador 1)
          (set! jugador 2)
          (set! jugador 1))
      (msj (string-append "Gano el jugador: " (number->string jugador)))))

;Mueve una ficha del tablero 
(define (moverFichaTablero row col rowMove colMove)
  (define a (+ (* row 80) margen))
  (define b (+ (* col 80) margen))
  (define t (+ (* rowMove 80) margen))
  (define d (+ (* colMove 80) margen))
  ((draw-solid-rectangle p ) (make-posn (+ b 12) (+ a 5)) 60 60 "white")
  (copy-viewport p z)
  (if(equal? jugador 1)
     (begin ((draw-pixmap p) "ficha1.png"(make-posn  (+ d 22) (+ t 15)))
            (copy-viewport p z))
     (begin ((draw-pixmap p) "ficha2.png"(make-posn  (+ d 22) (+ t 15)))
            (copy-viewport p z))))



;espera que el usuario clickee sobre una ficha del jugador actual
(define (waitPosition x y)
   (if (equal? (left-mouse-click? (get-mouse-click z)) #f)
       (waitPosition x y)
       (begin
        (set! x2 (pos (posn-x (query-mouse-posn z))))
        (set! y2 (pos (posn-y (query-mouse-posn z))))
        (cond
          ((and (equal? x x2) (equal? y y2)) (juego))
          ((or (or (>(posn-x(query-mouse-posn z))485)(<(posn-x(query-mouse-posn z))10))(or(>(posn-y(query-mouse-posn z))405)(<(posn-y(query-mouse-posn z))10)))(juego))
          ;((not (equal? (abs (- x x2)) (abs (- y y2)))) (msj MN))
          (else (verificarMovimiento x y y2 x2)))
         (juego))))

;verifica si una direccion que se clickeo es posible moverse para el usuario, sino sigue esperando
(define (MoverFichaUser x y)
  (if (canMove matrizGame x y)
      (waitPosition x y)
      (juego)))

;Alerta
(define (msj text)
  (define m (open-viewport "Fin"  300 50))
  ((draw-string m) (make-posn 50 20) text "red")
  (sleep 4)
  (close-viewport m)
  (exit))

;Para por 5 segundos para que sea visible para el usuario, crea el arbol, el minimax se encarga de tomar el mejor nodo del arbol,
;seguidamente se manda a mover el movimiento seleccionado por el miniMax
(define (juegoIA)
  (sleep 5)
  (define nodoMovimiento ( minMaxTree (makeTree matrizGame jugador)))
  (moveStamp (nodoInicioRow nodoMovimiento) (nodoInicioCol nodoMovimiento) (rowMove (nodoInicioRow nodoMovimiento) (nodoInicioDir nodoMovimiento)) (colMove (nodoInicioCol nodoMovimiento) (nodoInicioDir nodoMovimiento))))

;Manda a remover una ficha del contrincante
(define (removeIA)
  (sleep 5)
  (if (equal? IA 1)
      (removeIAAux 2)
      (removeIAAux 1))) 

;De igual forma para validar el mejor movimiento, el miniMax se encarga de verificar cual es la siguiente mejor jugada del contrario,
;esa es la ficha que la IA va a quitar del tablero
(define (removeIAAux playerContrario)
  (define nodoMovimiento ( minMaxTree (makeTree matrizGame playerContrario)))
  (removePos (nodoInicioRow nodoMovimiento) (nodoInicioCol nodoMovimiento))
  (juego))

;Funcion principal de juego, verifica cual es el jugador, si es la IA se va por un camino, si es un usuario espera el click sobre una ficha del usuario
;si no empieza el juego de la IA
(define (juego)
  (if (equal? IA jugador)
      (juegoIA)
      (if (equal? (left-mouse-click? (get-mouse-click z)) #f)
      (juego)
      (begin
        (set! x2 (pos (posn-x (query-mouse-posn z))))
        (set! y2 (pos (posn-y (query-mouse-posn z))))
        (cond
          ((and (equal? x x2) (equal? y y2)) (msj PS))
          ((or (or (>(posn-x(query-mouse-posn z))485)(<(posn-x(query-mouse-posn z))10))(or(>(posn-y(query-mouse-posn z))405)(<(posn-y(query-mouse-posn z))10)))(msj FT))
          ;((not (equal? (abs (- x x2)) (abs (- y y2)))) (msj MN))
          (else (verificarJugador y2 x2)))
         (juego)))))

;verifica la cantidad de fichas de un jugador, o cuantas veces hay un numero en la matriz
(define (amountStamps player row col count)
  (if (equal? row 4)
      (if (equal? col 5)
          (if (equal? player (matrix-ref matrizGame row col))
              (+ count 1)
              count)
          (if (equal? player (matrix-ref matrizGame row col))
              (amountStamps player row (+ col 1) (+ count 1))
              (amountStamps player row (+ col 1) count)))
      (if (equal? col 5)
          (if (equal? player (matrix-ref matrizGame row col))
              (amountStamps player (+ row 1) 0 (+ count 1))
              (amountStamps player (+ row 1) 0 count))
          (if (equal? player (matrix-ref matrizGame row col))
              (amountStamps player row (+ col 1) (+ count 1))
              (amountStamps player row (+ col 1) count)))))

;verifica si hay un ganador, retorna el ganador, si no hay es 0
(define (verifyWinner)
  (if (<= (amountStamps 1 0 0 0) 2) ;si la cantidad de fichas del jugador 1 es 2 o menos
      2                              ;gana el jugador 2
      (if (<= (amountStamps 2 0 0 0) 2) ;sino, si el jugador 2 tiene menos de 3 fichas
          1                          ;gana el jugador 1
          0 )))                          ;sino no hay ganador
 

;Se encarga de validar si en la posición que clickeo el jugador es una ficha suya
( define (verificarJugador x y)
   (if (equal? (matrix-ref matrizGame x y) jugador)
       (MoverFichaUser x y )
       (juego)))

(matriz-auto)
(set! IA (gna 1 2))
(set! jugador (gna 1 2))
(encontrarCordenanda)
(draw-board)
(juego)




