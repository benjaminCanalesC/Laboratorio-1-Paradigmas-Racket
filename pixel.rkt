#lang racket

(provide getXposition)
(provide getYposition)
(provide getPixelValue)
(provide getDepth)

;TDA pixel

;selectores

;Descripción: funcion que retorna la posición en X del pixel entrante
;Dominio: pixbit-d | pixhex-d | pixrgb-d
;Recorrido: posX (int)
;Recursión: no aplica
(define getXposition car)

;Descripción: funcion que retorna la posición en Y del pixel entrante
;Dominio: pixbit-d | pixhex-d | pixrgb-d
;Recorrido: posY (int)
;Recursión: no aplica
(define getYposition cadr)

;Descripción: funcion que obtiene el valor de un pixel entrante
;Dominio: pixbit-d | pixhex-d | pixrgb-d
;Recorrido: int (0|1) | string | list (r g b)
;Recursión: no aplica
(define getPixelValue (lambda (pixel)
                        (if (= (length (cddr (reverse (cdr (reverse pixel))))) 3)
                            (cddr (reverse (cdr (reverse pixel))))
                            (caddr (reverse (cdr (reverse pixel)))))))

;Descripción: funcion que retorna la profundidad del pixel entrante
;Dominio: pixbit-d | pixhex-d | pixrgb-d
;Recorrido: profundidad (int)
;Recursión: no aplica
(define getDepth (lambda (pixel)
                   (car (reverse pixel))))