#lang racket
(require racket/list)
(require data/spmatrix)
(require racket/system)


;FAuncion a importar, la cual evalua de 2 formas un movimiento, verifica 2 factores 1 positivo y uno negativo, los suma y es lo que devuelve
(define (evalMove matrizMove rowM colM row col  player gen)
  (define evaluacion 0)
  (set! evaluacion (+ evaluacion (evalPositive matrizMove rowM colM player gen)))
  (set! evaluacion (+ evaluacion (evalNegative matrizMove row col rowM colM player gen)))
  evaluacion)

;Sub evaluacion negativa:
     ; 1- Evalua que se habilite un trío del contrario, de ser así se restan 15 puntos, sino 0
     ; 2- De no haber tríos habilitados, evaluamos si hay dúos(2 fichas juntas) habilitadas del contrario, de ser así restamos 5 puntos por cada duo bloqueado
     ; 3- Evalua si al movernos perdemos duos(2 fichas juntas) a favor, de ser así restamos 5 puntos por cada duo perdido
(define (evalNegative matrizMove row col rowM colM player gen)
  (define evaluacion 0)
  (define playerContra 0)
  (if (equal? player 1)
      (set! playerContra (+ playerContra 2))
      (set! playerContra (+ playerContra 1)))
  (if(equal? (evalHabilitarPosibleTrioContra matrizMove row col playerContra gen) 0) ;si habilitamos el trio evaluamos trio, sino evaluamos duo 
     (set! evaluacion (+ evaluacion (evalHabilitarPosibleDuoContraFavor matrizMove row col playerContra gen))) ; habilitar 2 fichas seguidas contrincante
     (set! evaluacion (+ evaluacion (evalHabilitarPosibleTrioContra matrizMove row col playerContra gen))) ; habilitar 3 fichas seguidas contrincante
     )
  (set! evaluacion (+ evaluacion (evalHabilitarPosibleDuoContraFavor matrizMove rowM colM player gen))) ;evaluar si perdimos duo
  (set! evaluacion (+ evaluacion (evalHabilitarPosibleTrioFavorPerdido matrizMove row col player gen)))
  evaluacion) 

;verificamos si habilitamos duos (2 fichas seguidas) del contrincante, -5 por cada ficha, esto en el espacio que dejamos libre
(define (evalHabilitarPosibleDuoContraFavor matrizMove row col player gen)
  (* -5 (fichasRodea matrizMove row col player))
)

;evalua si movemos una ficha y perdemos un trio
(define (evalHabilitarPosibleTrioFavorPerdido matrizMove row col player gen)
  (if (validate-pos matrizMove row col player)
      0
      10))

;verificamos si habilitamos trios (3 fichas seguidas del contrincante), -15 puntos, esto en el espacio libre que dejamos
(define (evalHabilitarPosibleTrioContra matrizMove row col player gen)
  (if (validate-pos matrizMove row col player)
      0
      20))


;subEvaluacion positiva, toma 4 enfoques a evaluar:
     ; 1- Evalua que se bloquee un trío del contrario, de ser así se suman 10 puntos, sino 0
     ; 2- De no haber tríos bloqueados, evaluamos si hay dúos(2 fichas juntas) bloqueadas del contrario, de ser así suma 5 puntos por cada duo bloqueado
     ; 3- Evalua si al movernos habilitamos tríos(3 fichas juntas) a favor, de ser así sumamos 15 puntos
     ; 4- Evalua si al movernos habilitamos 1 o más duos(2 fichas juntas) a favor, de ser así sumamos 5 puntos por cada dúo habilitado
(define (evalPositive matrizMove rowM colM player gen)
  (define evaluacion 0)
  (define playerContra 0)
  (if (equal? player 1)
      (set! playerContra (+ playerContra 2))
      (set! playerContra (+ playerContra 1)))
  (if(equal? (evalBloquearTrioContra matrizMove rowM colM playerContra gen) 0) ;evaluamos trio si tapamos un trio del contrincante, sino evaluamos duo
     (set! evaluacion (+ evaluacion (evalBloquearDuoContra matrizMove rowM colM playerContra gen))) ;evaluamos si bloqueamos duo 
     (set! evaluacion (+ evaluacion (evalBloquearTrioContra matrizMove rowM colM playerContra gen)))) ;evaluamos si bloqueamos trio 
  (if (equal? (evalTrioFavorable matrizMove rowM colM player gen) 0)
      (set! evaluacion (+ evaluacion (evalDuoFavorable matrizMove rowM colM player gen))); habilitar 2 fichas seguidas a favor
      (set! evaluacion (+ evaluacion (evalTrioFavorable matrizMove rowM colM player gen)))); habilitar 3 fichas seguidas a favor 
  evaluacion) 

;Evaluamos si hay un posible trio a favor en la posicion que nos movimos
(define (evalTrioFavorable matrizMove row col player gen)
  (if (validate-pos matrizMove row col player)
      0
      (if (equal? gen 1)
          55
          20)))

;Evaluamos si hay un posible duo a favor en la posicion que nos movimos
(define (evalDuoFavorable matrizMove rowM colM player gen)
   (* 5 (fichasRodea matrizMove rowM colM player))) ;aqui cabe la posibilidad que haya 2 duos, ejemplo (arriba-centro-derecha) (izquierda-centro-abajo)
      

;Evaluamos si hay un posible trio contrario en la posicion que nos movimos, es decir, tapamos un trio del contrario
(define (evalBloquearTrioContra matrizMove row col player gen)
  (if (validate-pos matrizMove row col player)
      0
      15))

;Evaluamos si hay un posible duo contrario en la posicion que nos movimos, es decir, tapamos un trio del contrario
(define (evalBloquearDuoContra matrizMove rowM colM player gen)
  (* 5 (fichasRodea matrizMove rowM colM player))) ;aqui cabe la posibilidad que haya 2 duos, ejemplo (arriba-centro-derecha) (izquierda-centro-abajo)


;Funcion que retorna cuantas fichas al rededor del player entrante hay en la posición row col de la matrizMove
(define (fichasRodea matrizMove row col player)
  (define evaluacion 0)
  (set! evaluacion (+ evaluacion (rodeoArriba matrizMove row col player)))
  (set! evaluacion (+ evaluacion (rodeoAbajo matrizMove row col player)))
  (set! evaluacion (+ evaluacion (rodeoIzquierda matrizMove row col player)))
  (set! evaluacion (+ evaluacion (rodeoDerecha matrizMove row col player)))
  evaluacion)

;Funciones que verifican si en las posiciones arriba, abajo, izquierda y derecha hay una ficha del player en la matriz respectivamente
(define (rodeoArriba matrizMove row col player)
  (if (equal? row 0)
      0
      (if (equal? player (matrix-ref matrizMove (- row 1)col))
          1
          0)))

(define (rodeoAbajo matrizMove row col player)
  (if (equal? row 4)
      0
      (if (equal? player (matrix-ref matrizMove (+ row 1)col))
          1
          0)))

(define (rodeoIzquierda matrizMove row col player)
  (if (equal? col 0)
      0
      (if (equal? player (matrix-ref matrizMove row (- col 1)))
          1
          0)))

(define (rodeoDerecha matrizMove row col player)
  (if (equal? col 5)
      0
      (if (equal? player (matrix-ref matrizMove row (+ col 1)))
          1
          0)))

  
;#####################################################
;############### Validaciones

;Segmento de funciones donde se validan las posiciones en las que se puede colocar la ficha de jugador 1 o 2
;Validar las posiciones, izquierda, derecha, arriba, abajo, arriba-abajo, izquierda-derecha

;Valida si una posición puede ser colocada, esto para la colocación aleatoria o manual, osea que no sea posible poner 3 fichas juntas en estas direcciones
(define (validate-pos matriz row col num)
  (if (validate-top matriz row col num)
      (if (validate-bot matriz row col num)
        (if (validate-left matriz  row col num)
             (if (validate-right matriz row col num)
                 (if (validate-top-bot matriz row col num)
                     (if (validate-left-right matriz row col num)
                         #t
                         #f)
                     #f)
                 #f)
             #f)
         #f)
      #f))

(define (validate-top-bot matriz row col num)
  (if (equal? row 0) 
      #t
      (if (equal? row 4) 
          #t
          (if (equal? num (matrix-ref matriz (- row 1) col))
              (if (equal? num (matrix-ref matriz (+ row 1) col))
                  #f
                  #t)
              #t))))

(define (validate-left-right matriz row col num)
  (if (equal? col 0) 
      #t
      (if (equal? col 5)
          #t
          (if (equal? num (matrix-ref matriz row (- col 1)))
              (if (equal? num (matrix-ref matriz row (+ col 1)))
                  #f
                  #t)
              #t))))

(define (validate-top matriz row col num)
  (if (<= row 1) 
      #t
      (if (equal? num (matrix-ref matriz (- row 1) col))
          (if (equal? num (matrix-ref matriz (- row 2) col))
              #f
              #t)
          #t)))

(define (validate-bot matriz row col num)
  (if (>= row 3) 
      #t
      (if (equal? num (matrix-ref matriz (+ row 1) col))
          (if (equal? num (matrix-ref matriz (+ row 2) col))
              #f
              #t)
          #t)))

(define (validate-left matriz row col num)
  (if (<= col 1) 
      #t
      (if (equal? num (matrix-ref matriz row (- col 1)))
          (if (equal? num (matrix-ref matriz row (- col 2)))
              #f
              #t)
          #t)))

(define (validate-right matriz row col num)
  (if (>= col 4) 
      #t
      (if (equal? num (matrix-ref matriz row (+ col 1)))
          (if (equal? num (matrix-ref matriz row (+ col 2)))
              #f
              #t)
          #t)))

;########################## Fin de segmento de validaciones #########################


(provide validate-pos)
(provide evalMove)





