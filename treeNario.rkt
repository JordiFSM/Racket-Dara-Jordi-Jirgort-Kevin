#lang racket
(require racket/list)
(require data/spmatrix)
(require racket/system)
(require racket/struct)
(require "eval.rkt")

(define-struct tree (nodoRaiz jugador tiempos eval)#:transparent) ;arbol de juego, es el que guardará la estructura que vamos a utilizar
(define-struct nodo (inicio row col direccion puntuacion generacion jugador matriz matrizMove hijos)#:transparent);podria decirse que es el agente del miniMax, encargado de guardar sus coordenadas, su inicio y la puntuación que estos movimientos reciben segun la funcion eval
(define-struct nodoRaiz (jugador matriz hijos)#:transparent) ;Nodo raiz del arbo, guarda la primer generacion de movimientos
(define-struct inicio (row col direccion)#:transparent) ;guarda cual es el movimiento de la generación 1 de donde viene
(define IA 0)
(define listaEval '())

;Funcion encargada de crear el arbol a partira de una matriz y un numero, que este será el jugador
(define (makeTree matriz jugador)
  (set! IA jugador)
  (make-tree (makeNodoRaiz matriz jugador) jugador listaEval))

;crea el nodoRaiz que se guardará en el arbol
(define (makeNodoRaiz matriz jugador)
  (make-nodoRaiz jugador matriz (makeChilds matriz jugador 0 0 1 '() null 0)))

;Funcion encargada de crear el nodo de forma recursiva(Ramas) o final(Hojas), ademas de guardar en cada uno de estos la posición original de donde viene su trayectoria,
;y llevar un conteo de la puntuación o calificación que le vaya dando la funcion eval al nodo, si es una generación del jugador, osea que la jugada es posible crearla suma
;la puntuación que le de el eval, de ser una generación del jugador contrario se va restar esta puntuación, para bien o para mal
(define (makeNodo matriz jugador row col generacion direccion inicioP puntuacionTotal)
  (define puntuacionTotal2 0)
  (define inicioT (make-inicio 0 0 0))
  ;eval inicio 
  (if (equal? IA jugador)
      (set! puntuacionTotal2 (+ puntuacionTotal (evalMove (updateMatriz (clone-matrix matriz) row col jugador direccion) (rowMove row direccion) (colMove col direccion) row col  jugador generacion)))
      (set! puntuacionTotal2 (- puntuacionTotal (evalMove (updateMatriz (clone-matrix matriz) row col jugador direccion) (rowMove row direccion) (colMove col direccion) row col  jugador generacion))))
  ;eval fin
  ;agregar a listaEval
  (if (equal? generacion 1)
      (set! inicioT (make-inicio row col direccion))
      (set! inicioT inicioP))
  (if (< generacion 4)
           (make-nodo inicioT row col direccion puntuacionTotal2 generacion jugador matriz (updateMatriz (clone-matrix matriz) row col jugador direccion) (makeChilds (updateMatriz (clone-matrix matriz) row col jugador direccion) (changePlayer jugador) 0 0 (+ generacion 1) '() inicioT puntuacionTotal2)) ;<----- recursiva
           (make-nodo inicioT row col direccion puntuacionTotal2 generacion jugador matriz (updateMatriz (clone-matrix matriz) row col jugador direccion) '()))) ;<----- sin recursividad, parada en profundidad 3
           

    ;^^(evalMove (updateMatriz (clone-matrix matriz) row col jugador direccion) (rowMove row direccion) (colMove col direccion) row col  jugador)
    ;^^Esta parte del código lo que pide es la puntuación que se crea con la función eval
    ;^^(updateMatriz (clone-matrix matriz) row col jugador direccion)
    ;^^Esta parte del código lo que hace es colnar la matriz y cambiarle la posición del movimiento
    ;^^(makeChilds (updateMatriz (clone-matrix matriz) row col jugador direccion) (changePlayer jugador) 0 0 (+ generacion 1) '())
    ;^^Esta parte del código lo que hace es crear la lista de hijos de este nodo, es la parte recursiva del código de creacion del arbol

;Funcion que actualiza la matriz, cambia una posición segun el row col 
(define (updateMatriz matrizParam row col jugador direccion)
  (if (equal? direccion 1)
      (setPos matrizParam (- row 1) col row col jugador)
      (if (equal? direccion 2)
          (setPos matrizParam (+ row 1) col row col jugador)
          (if (equal? direccion 3)
              (setPos matrizParam row (- col 1) row col jugador)
              (if (equal? direccion 4)
                  (setPos matrizParam row (+ col 1) row col jugador)
                  (display("Aqui estaba el error"))))))
  matrizParam)

;funcion encargada de setear una posición en una matriz
(define (setPos matrizParam rowN colN row col num)
  (matrix-set! matrizParam row col 0)
  (matrix-set! matrizParam rowN colN num))

;Funcion encargada de cambiar el jugador
(define (changePlayer jugador)
  (if (equal? jugador 1)
      2
      1))

;Funcion que pregunta si la fila y la columna estan fuera de rango entonces se termina, sino va a si en la posicion row col existen movimientos disponibles
(define (makeChilds matriz jugador row col generacion lista inicio puntuacionTotal)
  (if (rowcol50 row col)
       lista
      (findChilds matriz jugador row col generacion lista inicio puntuacionTotal)))

;crea un nodo si en la posicion row col esta una ficha del jugador y lo agrega a la lista
(define (findChilds matriz jugador row col generacion lista inicio puntuacionTotal)
  (if (equal? row 5)
      (makeChilds matriz jugador row col generacion lista inicio puntuacionTotal)
      (if (equal? (matrix-ref matriz row col) jugador)
          (findChildsDirection matriz jugador row col generacion lista 1 inicio puntuacionTotal) ;Si es ficha, va a buscar posibles direcciones, el 1 es la posicion 1 = arriba, toca buscar las otras 3
          (findChildsAux matriz jugador row col generacion lista inicio puntuacionTotal))))         ; Si no es hijo pasa de posición para seguir buscando o terminar

;Pasa de fila o columnas y sigue buscando hijos
(define (findChildsAux matriz jugador row col generacion lista inicio puntuacionTotal)
  (if (equal? col 5) ;Si la columna es la última 
      (findChilds matriz jugador (+ row 1) 0 generacion lista inicio puntuacionTotal) ;sube la fila pero baja la columna a 0
      (findChilds matriz jugador row (+ col 1) generacion lista inicio puntuacionTotal))) ;sino solo sube la columna
      
;Funcion que verifica si hay movimiento para cada una de las posiciones, si hay crea un nodo en esa posición y lo añade a la lista, sino pasa a la siguiente direccion hasta terminar
; cuando termina pasa a la siguiente posición a buscar más hijos 
(define (findChildsDirection matriz jugador row col generacion lista posicion inicio puntuacionTotal)
  (if(equal? posicion 1) ;<-Arriba
     (if (topVoid matriz row col)
         (findChildsDirectionAux matriz jugador row col generacion lista posicion inicio puntuacionTotal)  ;<-se cre un child direcion arriba
         (findChildsDirection matriz jugador row col generacion lista 2 inicio puntuacionTotal)) ;<-se pasa de posicion sin crear
     (if(equal? posicion 2) ;<-Abajo
        (if (botVoid matriz row col)
            (findChildsDirectionAux matriz jugador row col generacion lista posicion inicio puntuacionTotal) ;<-se cre un child direcion abajo
            (findChildsDirection matriz jugador row col generacion lista 3 inicio puntuacionTotal)) ;<-se pasa de posicion sin crear
        (if(equal? posicion 3) ;<-Izquierda
           (if (leftVoid matriz row col)
               (findChildsDirectionAux matriz jugador row col generacion lista posicion inicio puntuacionTotal) ;<-se cre un child direcion iazquierda
               (findChildsDirection matriz jugador row col generacion lista 4 inicio puntuacionTotal)) ;<-se pasa de posicion sin crear
           (if (rightVoid matriz row col)
                   (findChildsDirectionAux matriz jugador row col generacion lista posicion inicio puntuacionTotal) ;<-se cre un child direcion derecha
                   (findChildsAux matriz jugador row col generacion lista inicio puntuacionTotal)))))) ;<-se pasa de posicion sin crear                    

;Funcion auxiliar de findDirection, se encarga de verificar si es la ultima dirección posible hacia donde moverse, sino sigue buscando direcciones
(define (findChildsDirectionAux matriz jugador row col generacion lista posicion inicio puntuacionTotal)
  (if (equal? posicion 4)
      (findChildsAux matriz jugador row col generacion (append lista (list (makeNodo matriz jugador row col generacion posicion inicio puntuacionTotal))) inicio puntuacionTotal ) ;<<insertar nodo a lista y pasar posicion
      (findChildsDirection matriz jugador row col generacion (append lista (list (makeNodo matriz jugador row col generacion posicion inicio puntuacionTotal))) (+ posicion 1) inicio puntuacionTotal))) ;<< insertar nodo a lista y buscar en otra direccion
      
  
;valida que row col sea 5 0 
(define (rowcol50 row col)
  (if (equal? row 5)
      (if (equal? col 0)
          #t
          #f)
      #f))

;#############################################
;##################### Clonar matriz

;Funcion que se encarga clonar una matriz que recibe
(define (clone-matrix matriz)
  (define matriz5x6 (build-matrix 5 6 (lambda (i j) 0)))
  (copy-matrix matriz matriz5x6 0 0)
   matriz5x6)

;funcion que se encarga de copiar todas las fila columna de una matriz en otra de forma recursiva
(define (copy-matrix matriz matrizClone row col)
  (matrix-set! matrizClone row col (jugadorPos matriz row col))
  (if (equal? row 4)
      (if (equal? col 5)
          (matrix-set! matrizClone row col (jugadorPos matriz row col))
          (copy-matrix matriz matrizClone row (+ col 1)))
      (if (equal? col 5)
          (copy-matrix matriz matrizClone (+ row 1) 0)
          (copy-matrix matriz matrizClone row (+ col 1)))))

;funcion que se encarga de verificar cual es el jugador de una fila columna
(define (jugadorPos matriz row col)
  (if (equal? (matrix-ref matriz row col) 1)
      1
      (if (equal? (matrix-ref matriz row col) 0)
          0
          2)))

;#############################################
;################# verificar posiciones vacias

;Verifica que sea posible moverse en cualquier dirección
(define (canMove matriz row col)
  (if (topVoid matriz row col)
      #t
      (if (botVoid matriz row col)
          #t
          (if (leftVoid matriz row col)
              #t
              (if (rightVoid matriz row col)
                  #t
                  #f)))))

;#############################################
;Devuelve un numero dependiendo de si la row col original comparada a la row col move es un movimiento hacia una dirección, sino da cero
(define (giveMove rowO colO rowM colM)
  (if (esTop rowO colO rowM colM)
      1
      (if (esBot rowO colO rowM colM)
          2
          (if (esLeft rowO colO rowM colM)
              3
              (if esRight
                  4
                  0)))))

;Pregunta si la row col original es la row col hacia una posición específica, top, bot, left, right
(define (esTop rowO colO rowM colM)
  (if (equal? rowO 0)
      #f
      (if (equal? (- rowO 1) rowM )
          (if (equal? colO colM)
              #t
              #f)
          #f)))

;Pregunta si la row col original es la row col hacia una posición específica, top, bot, left, right
(define (esBot rowO colO rowM colM)
  (if (equal? rowO 4)
      #f
      (if (equal? (+ rowO 1) rowM )
          (if (equal? colO colM)
              #t
              #f)
          #f)))

;Pregunta si la row col original es la row col hacia una posición específica, top, bot, left, right
(define (esLeft rowO colO rowM colM)
  (if (equal? colO 0)
      #f
      (if (equal? (- colO 1) colM )
          (if (equal? rowO rowM)
              #t
              #f)
          #f)))

;Pregunta si la row col original es la row col hacia una posición específica, top, bot, left, right
(define (esRight rowO colO rowM colM)
  (if (equal? colO 5)
      #f
      (if (equal? (+ colO 1) colM )
          (if (equal? rowO rowM)
              #t
              #f)
          #f)))

;Verifica que la posición arriba seleccionada sea posible moverse
(define (topVoid matriz row col)
  (if (equal? row 0)
      #f
      (if (equal? (matrix-ref matriz (- row 1) col) 0)
          #t
          #f)))

;Verifica que la posición abajo seleccionada sea posible moverse
(define (botVoid matriz row col)
  (if (equal? row 4)
      #f
      (if (equal? (matrix-ref matriz (+ row 1) col) 0)
          #t
          #f)))

;Verifica que la posición a la izquierda seleccionada sea posible moverse
(define (leftVoid matriz row col)
  (if (equal? col 0)
      #f
      (if (equal? (matrix-ref matriz row (- col 1)) 0)
          #t
          #f)))

;Verifica que la posición a la derecha seleccionada sea posible moverse
(define (rightVoid matriz row col)
  (if (equal? col 5)
      #f
      (if (equal? (matrix-ref matriz row (+ col 1)) 0)
          #t
          #f)))

;recibe un arbol y envia a imprimir los hijos de su nodo raiz
(define (printTree tree)
  (printNodosR (tree-nodoRaiz tree)))

;envia la lista de nodos del nodo raiz a imprimirse
(define (printNodosR nodo)
  (if (empty? (nodoRaiz-hijos nodo))
      ""
      (printNodos(nodoRaiz-hijos nodo))))


;verifica que la lista no este vacia
(define (printNodos nodos)
  (if (empty? nodos)
      ""
      (printList (car nodos) (cdr nodos))))

;imprime la informacion e imprime los hijos del nodo de forma recursiva
(define (printList nodo lista)
  (displayln (string-append "\nNodo " (number->string (nodo-row nodo))"," (number->string (nodo-col nodo)) " direción: " (number->string (nodo-direccion nodo)) " generacion: " (number->string (nodo-generacion nodo)) " jugador: " (number->string (nodo-jugador nodo)) " puntuación: " (number->string (nodo-puntuacion nodo))))
  (displayln "MatrizPadre")
  (display-matrix (nodo-matriz nodo))
  (displayln "MatrizMove")
  (display-matrix (nodo-matrizMove nodo))
  (displayln "")
  (printNodos (nodo-hijos nodo))
  (if (empty? lista)
      (display "\nFin\n")
      (printNodos lista)))

;cambia el row segun el movimiento
(define (rowMove row move)
  (if (equal? move 1)
      (- row 1)
      (if (equal? move 2)
          (+ row 1)
          row)))

;cambia el col segun el movimiento, si no se mueve queda igual
(define (colMove col move)
  (if (equal? move 3)
      (- col 1)
      (if (equal? move 4)
          (+ col 1)
          col)))

;crear nodos vacios o con ciertas caracteristicas, necesarios para el alpha y beta del miniMax-alpha-beta
(define (makeNodoVoid tree)
  (make-nodo null 0 0 0 0 0 0 null null (nodoRaiz-hijos (tree-nodoRaiz tree))))

(define (makeNodoVoid2)
  (make-nodo null 0 0 0 0 0 0 null null '()))

(define (makeNodoVoid3 num)
  (make-nodo null 0 0 0 num 0 0 null null '()))

;geters de los structs
(define (nodoInicioRow nodo)
  (inicio-row (nodo-inicio nodo)))

(define (nodoInicioCol nodo)
  (inicio-col (nodo-inicio nodo)))

(define (nodoInicioDir nodo)
  (inicio-direccion (nodo-inicio nodo)))

(define (listaNodo nodo)
  (nodo-hijos nodo))

(define (puntuacionNodo nodo)
  (nodo-puntuacion nodo))

;pregunta si una posición puede moverse hacia cierta direccion, es decir no hay fichas en esa posición
(define (canMoveDir matriz row col dir)
  (if (equal? dir 1)
      (if (topVoid matriz row col)
          #t
          #f)
      (if (equal? dir 2)
      (if (botVoid matriz row col)
          #t
          #f)
      (if (equal? dir 3)
          (if (leftVoid matriz row col)
              #t
              #f)
          (if (equal? dir 4)
              (if (rightVoid matriz row col)
                  #t
                  #f)
              #f)))))

(provide rowMove)
(provide colMove)
(provide nodoInicioRow)
(provide nodoInicioCol)
(provide nodoInicioDir)
(provide canMoveDir)
(provide listaNodo)
(provide puntuacionNodo)
(provide makeNodoVoid)
(provide makeNodoVoid2)
(provide makeNodoVoid3)
(provide printTree)
(provide canMove)
(provide topVoid)
(provide botVoid)
(provide leftVoid)
(provide rightVoid)
(provide makeTree)
(provide giveMove)
