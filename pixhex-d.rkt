#lang racket

(require "pixel.rkt")
(provide pixhex-d)
(provide pixhex?)
(provide pixhex->string)

;TDA pixhex-d

;constructor

;Descripción: funcion constructora de un pixhex-d
;Dominio: x (int), y (int), hex (string), profundidad (int)
;Recorrido: pixhex-d
;Recursión: no aplica
(define pixhex-d(lambda (x y hex d) (list x y hex d)))

;pertenencia

;Descripción: funcion de pertenencia que analiza un pixel entrante para determinar si es un pixhex-d
;Dominio: pixhex-d
;Recorrido: boolean
;Recursión: no aplica
(define pixhex? (lambda (pixel)
                  ;analiza las condiciones primitivas de los pixeles pixhex-d
                  (if (and (= (length pixel) 4) (integer? (getXposition pixel)) (integer? (getYposition pixel)) (string? (getPixelValue pixel)) (integer? (getDepth pixel)))
                                      #t
                                      #f)))


;otros

;Descripción: funcion que retorna en formato string el valor del pixhex-d entrante
;Dominio: pixhex-d
;Recorrido: string
;Recursión: no aplica
(define pixhex->string (lambda (pixel)
                         ;mediante una funcion auxiliar que genera la recursion
                         (getPixelValue pixel)
                         )
  )