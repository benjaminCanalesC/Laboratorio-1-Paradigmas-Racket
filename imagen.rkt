#lang racket

(provide getWidthImage)
(provide getHeightImage)
(provide getImagePixels)

;TDA imagen

;selectores

;Descripción: funcion que retorna el ancho de una imagen entrante
;Dominio: imagen
;Recorrido: ancho (int)
;Recursión: no aplica
(define getWidthImage car)

;Descripción: funcion que retorna el alto de una imagen entrante
;Dominio: imagen
;Recorrido: alto (int)
;Recursión: no aplica
(define getHeightImage cadr)

;Descripción: funcion que retorna los pixeles de una imagen entrante
;Dominio: imagen
;Recorrido: pixeles (list)
;Recursión: no aplica
(define getImagePixels caddr)