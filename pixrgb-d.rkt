#lang racket

(require "pixel.rkt")

(provide pixrgb-d)
(provide pixrgb?)
(provide getR)
(provide getG)
(provide getB)
(provide RGBvaluesTest)
(provide incCh)
(provide setR)
(provide setG)
(provide setB)
(provide pixrgb->string)

;TDA pixrgb-d

;constructor

;Descripción: funcion constructora de un pixrgb-d
;Dominio: x (int), y (int), r (int), g (int), b (int), profundidad (int)
;Recorrido: pixrgb-d
;Recursión: no aplica
(define pixrgb-d(lambda (x y r g b d) (list x y r g b d)))


;pertenencia

;Descripción: funcion de pertenencia que analiza un pixel entrante para determinar si es un pixrgb-d
;Dominio: pixrgb-d
;Recorrido: boolean
;Recursión: no aplica
(define pixrgb? (lambda (pixel)
                  ;analiza las condiciones primitivas de los pixeles pixrgb-d
                  (if (and (= (length pixel) 6) (integer? (getXposition pixel)) (integer? (getYposition pixel)) (if (= (if (list? (getPixelValue pixel)) (length (getPixelValue pixel)) -1) 3) (andmap RGBvaluesTest (getPixelValue pixel)) #f) (integer? (getDepth pixel)))
                                      #t
                                      #f)))

;selector

;Descripción: funcion que obtiene el valor respectivo al color rojo de un pixel rgb
;Dominio: pixrgb-d
;Recorrido: R (int)
;Recursión: no aplica
(define getR (lambda (RGBpixel) (caddr RGBpixel)))

;Descripción: funcion que obtiene el valor respectivo al color verde de un pixel rgb
;Dominio: pixrgb-d
;Recorrido: G (int)
;Recursión: no aplica
(define getG (lambda (RGBpixel) (cadddr RGBpixel)))

;Descripción: funcion que obtiene el valor respectivo al color blue de un pixel rgb
;Dominio: pixrgb-d
;Recorrido: B (int)
;Recursión: no aplica
(define getB(lambda (RGBpixel) (cadddr (cdr RGBpixel))))


;modificadores

;Descripción: funcion que modifica el color rojo de un pixel RGB
;Dominio: pixrgb-d
;Recorrido: pixrgb-d
;Recursión: no aplica
(define setR (lambda (pixelRGB Rvalue) (pixrgb-d (getXposition pixelRGB) (getYposition pixelRGB) Rvalue (getG pixelRGB) (getB pixelRGB) (getDepth pixelRGB))))

;Descripción: funcion que modifica el color verde de un pixel RGB
;Dominio: pixrgb-d
;Recorrido: pixrgb-d
;Recursión: no aplica
(define setG (lambda (pixelRGB Gvalue) (pixrgb-d (getXposition pixelRGB) (getYposition pixelRGB) (getR pixelRGB) Gvalue (getB pixelRGB) (getDepth pixelRGB))))

;Descripción: funcion que modifica el color azul de un pixel RGB
;Dominio: pixrgb-d
;Recorrido: pixrgb-d
;Recursión: no aplica
(define setB (lambda (pixelRGB Bvalue) (pixrgb-d (getXposition pixelRGB) (getYposition pixelRGB) (getR pixelRGB) (getG pixelRGB) Bvalue (getDepth pixelRGB))))


;otros

;Descripción: funcion que retorna en formato string el valor del pixrgb-d entrante
;Dominio: pixrgb-d
;Recorrido: string
;Recursión: no aplica
(define pixrgb->string (lambda (RGBpixel)
                         ;se define una funcion auxiliar en la cual se almacenan los valores hexadecimales respectivos
                         (define decimaltohex (lambda (decimal) (cond
                                                                  ((= decimal 0) (string #\0))
                                                                  ((= decimal 1) (string #\1))
                                                                  ((= decimal 2) (string #\2))
                                                                  ((= decimal 3) (string #\3))
                                                                  ((= decimal 4) (string #\4))
                                                                  ((= decimal 5) (string #\5))
                                                                  ((= decimal 6) (string #\6))
                                                                  ((= decimal 7) (string #\7))
                                                                  ((= decimal 8) (string #\8))
                                                                  ((= decimal 9) (string #\9))
                                                                  ((= decimal 10) (string #\A))
                                                                  ((= decimal 11) (string #\B))
                                                                  ((= decimal 12) (string #\C))
                                                                  ((= decimal 13) (string #\D))
                                                                  ((= decimal 14) (string #\E))
                                                                  ((= decimal 15) (string #\F))
                                                                  )
                                                )
                           )
                         ;funcion que convierte cada valor entrante R, G, B, en un valor Hexadecimal, mediante el uso de la division por 16
                         (define convert (lambda (value) (string-append (decimaltohex (quotient value 16)) (decimaltohex (remainder value 16)))))
                         ;la funcion auxiliar (convert) es aplicada a cada valor del pixel RGB
                         (string-append "" (convert (getR RGBpixel)) (convert (getG RGBpixel)) (convert (getB RGBpixel)))
                         )
  )

;Descripción: funcion que analiza el valor R, G, B
;Dominio: r (int) | g (int) | b (int)
;Recorrido: boolean
;Recursión: no aplica
(define RGBvaluesTest (lambda (RGBvalue) (and (<= RGBvalue 255) (>= RGBvalue 0))))

;Descripción: funcion que incrementa en una unidad el valor entrante
;Dominio: int
;Recorrido: int
;Recursión: no aplica
(define incCh (lambda (valor) (+ valor 1)))