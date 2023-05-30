#lang racket

(require "pixel.rkt")

(provide pixbit-d)
(provide pixbit?)
(provide getBitvalue)
(provide pixbit->string)


;TDA pixbit-d

;constructor

;Descripción: funcion constructora de un pixbit-d
;Dominio: x (int), y (int), bit (0|1), profundidad (int)
;Recorrido: pixbit-d
;Recursión: no aplica
(define pixbit-d (lambda (x y bit depth) (list x y bit depth)))

;pertenencia

;Descripción: funcion de pertenencia que analiza un pixel entrante para determinar si es un pixbit-d
;Dominio: pixbit-d
;Recorrido: boolean
;Recursión: no aplica
(define pixbit? (lambda (pixel)
                  ;analiza las condiciones primitivas de los pixeles pixbit-d
                  (if (and (= (length pixel) 4) (integer? (getXposition pixel)) (integer? (getYposition pixel)) (or (= (if (integer? (getPixelValue pixel)) (getPixelValue pixel) -1) 1) (= (if (integer? (getPixelValue pixel)) (getPixelValue pixel) -1) 0)) (integer? (getDepth pixel) ))
                                #t
                                #f)))

;selector

;Descripción: funcion que retorna el valor de un pixbit
;Dominio: pixbit-d
;Recorrido: bit (int 0|1) 
;Recursión: no aplica
(define getBitvalue third)

;otros

;Descripción: funcion que retorna en formato string el valor del pixbit-d entrante
;Dominio: pixbit-d
;Recorrido: string
;Recursión: no aplica
(define pixbit->string (lambda (pixel)
                         ;mediante una funcion auxiliar que genera la recursion
                         (number->string (getPixelValue pixel))
                         )
  )