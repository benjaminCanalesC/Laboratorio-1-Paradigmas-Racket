#lang racket

(require "pixel.rkt")
(require "pixbit-d.rkt")
(require "pixhex-d.rkt")
(require "pixrgb-d.rkt")
(require "imagen.rkt")

(provide image)
(provide bitmap?)
(provide hexmap?)
(provide pixmap?)
(provide compressed?)
(provide flipH)
(provide flipV)
(provide crop)
(provide imgRGB->imgHex)
(provide histogram)
(provide rotate90)
(provide compress)
(provide edit)
(provide invertColorBit)
(provide invertColorRGB)
(provide adjustChannel)
(provide image->string)

;Funcionalidades

;Descripción: funcion constructora de imagenes
;Dominio: ancho (int), alto (int), [pixbit-d | pixhex-d | pixrgb-d]
;Recorrido: imagen
;Recursión: no aplica
(define image (lambda (Width Heigth . Pixeles) (if (integer? (caar Pixeles))
                                                 (list Width Heigth (sort (sort Pixeles #:key getHeightImage <) #:key getWidthImage <))
                                                 (list Width Heigth (car (sort (sort Pixeles #:key getHeightImage <) #:key getWidthImage <))))))

;Descripción: funcion de pertenencia que analiza una imagen entrante para determinar si es un bitmap
;Dominio: imagen 
;Recorrido: boolean
;Recursión: no aplica
(define bitmap? (lambda (imagen)
                  (if(andmap pixbit? (getImagePixels imagen))
                     #t
                     #f)))

;Descripción: funcion de pertenencia que analiza una imagen entrante para determinar si es un hexmap
;Dominio: imagen 
;Recorrido: boolean
;Recursión: no aplica
(define hexmap? (lambda (imagen)
                  (if (andmap pixhex? (getImagePixels imagen))
                      #t
                      #f)))

;Descripción: funcion de pertenencia que analiza una imagen entrante para determinar si es un pixmap
;Dominio: imagen 
;Recorrido: boolean
;Recursión: no aplica
(define pixmap? (lambda (imagen)
                  (if (andmap pixrgb? (getImagePixels imagen))
                      #t
                      #f)))

;Descripción: funcion que analiza la compresión de una imagen
;Dominio: imagen
;Recorrido: boolean
;Recursión: no aplica
(define compressed? (lambda (imagen)
                      (if (and (or (bitmap? imagen) (hexmap? imagen) (pixmap? imagen)) (= (* (getWidthImage imagen) (getHeightImage imagen)) (length (getImagePixels imagen))))
                      #f
                      #t)))

;Descripción: funcion que invierte una imagen entrante de forma horizontal
;Dominio: imagen
;Recorrido: imagen
;Recursión: natural
;Motivo recursión: dejar un estado pendiente permite una creacion simple de la salida que se necesita
(define flipH (lambda (imagen)
                ;funcion auxiliar que permite invertir una fila de pixeles
                (define flip (lambda (pixeles)
                                 ;funcion auxiliar que permite intercambiar valores de posicion en Y de dos pixeles entrantes, generandolos a partir de su funcion constructora
                                 (define swapH (lambda (pixel1 pixel2)
                                                 (cond ((and (pixrgb? pixel1) (pixrgb? pixel2)) (list (pixrgb-d (getXposition pixel1) (getYposition pixel2) (getR pixel1) (getG pixel1) (getB pixel1) (getDepth pixel1)) (pixrgb-d (getXposition pixel2) (getYposition pixel1) (getR pixel2) (getG pixel2) (getB pixel2) (getDepth pixel2)) ))
                                                       ((and (pixbit? pixel1) (pixbit? pixel2)) (list (pixbit-d (getXposition pixel1) (getYposition pixel2) (getPixelValue pixel1) (getDepth pixel1)) (pixbit-d (getXposition pixel2) (getYposition pixel1) (getPixelValue pixel2) (getDepth pixel2))))
                                                       ((and (pixhex? pixel1) (pixhex? pixel2)) (list (pixhex-d (getXposition pixel1) (getYposition pixel2) (getPixelValue pixel1) (getDepth pixel1)) (pixhex-d (getXposition pixel2) (getYposition pixel1) (getPixelValue pixel2) (getDepth pixel2))))
                                                       )
                                                 )
                                   )
                               (if (or (null? pixeles) (= (length pixeles) 1))
                                     ;solución conocida: Se hace la diferencia entre caso para e impar
                                     ;caso par: retorna null
                                     ;caso impar: retorna el pixel sin modificar
                                     (if (null? pixeles)
                                         null
                                         pixeles)
                                     ;descomposición recursiva
                                     ;genera la recursión natural haciendo el llamado a una funcion auxiliar (swap) que genera el intercambio de pixeles (intercambio de posiciones) entre el primero y el ultimo de la fila o columna
                                     (append (swapH (first pixeles) (first (reverse pixeles))) (flip (reverse (cdr (reverse (cdr pixeles))))))
                                     )
                               )
                  )
                  ;se define una funcion auxiliar donde entran los pixeles y la fila donde se trabajará
                 (define AUX (lambda (pixeles posX)
                                ;condición de borde, si la fila analizada es el largo de la imagen:
                                (if (= posX (getHeightImage imagen))
                                    ;retorna null y procede a generar la salida
                                    null
                                    ;sino, realiza la recursión llamando a otra funcion auxiliar (flip) que logra invertir los pixeles, entregándole a esta los correspondientes a la fila analizada (filter) y el tipo de flip (int 0|1),
                                    ;agregando esto al llamado a sí misma generando la recursión natural
                                    (append (flip (filter (lambda (pixel) (= (getXposition pixel) posX)) pixeles)) (AUX pixeles (+ posX 1))) 
                                    )
                                 )
                    )
                  ;se genera la salida con la funcion constructora de imagenes haciendo el llamado a la auxiliar ordenando previamente los pixeles que salen de esta mediante la funcion sort
                  (image (getWidthImage imagen) (getHeightImage imagen) (sort (sort (AUX (getImagePixels imagen) 0) #:key getHeightImage <) #:key getWidthImage <))
                    )
  )

;Descripción: funcion que invierte una imagen entrante de forma vertical
;Dominio: imagen
;Recorrido: imagen
;Recursión: natural
;Motivo recursión: dejar un estado pendiente permite una creacion simple de la salida que se necesita
(define flipV (lambda (imagen)
                  ;funcion auxiliar que permite invertir una columna de pixeles
                  (define flip (lambda (pixeles)
                                 ;funcion auxiliar que permite intercambiar valores de posicion en X de dos pixeles entrantes
                                 (define swapV (lambda (pixel1 pixel2)
                                                 (cond ((and (pixrgb? pixel1) (pixrgb? pixel2)) (list (pixrgb-d (getXposition pixel2) (getYposition pixel1) (getR pixel1) (getG pixel1) (getB pixel1) (getDepth pixel1)) (pixrgb-d (getXposition pixel1) (getYposition pixel2) (getR pixel2) (getG pixel2) (getB pixel2) (getDepth pixel2)) ))
                                                       ((and (pixbit? pixel1) (pixbit? pixel2)) (list (pixbit-d (getXposition pixel2) (getYposition pixel1) (getPixelValue pixel1) (getDepth pixel1)) (pixbit-d (getXposition pixel1) (getYposition pixel2) (getPixelValue pixel2) (getDepth pixel2))))
                                                       ((and (pixhex? pixel1) (pixhex? pixel2)) (list (pixhex-d (getXposition pixel2) (getYposition pixel1) (getPixelValue pixel1) (getDepth pixel1)) (pixhex-d (getXposition pixel1) (getYposition pixel2) (getPixelValue pixel2) (getDepth pixel2))))
                                                       )
                                                 )
                                   )

                                 ;condicion de borde, para imagenes de ancho o largo par no restarán pixeles por invertir, para imagenes de medidas impares restará una fila o columna
                                 (if (or (null? pixeles) (= (length pixeles) 1))
                                     ;solución conocida: Se hace la diferencia entre caso para e impar
                                     ;caso par: retorna null
                                     ;caso impar: retorna el pixel sin modificar
                                     (if (null? pixeles)
                                         null
                                         pixeles)
                                     ;descomposición recursiva
                                     ;genera la recursión natural haciendo el llamado a una funcion auxiliar (swap) que genera el intercambio de pixeles (intercambio de posiciones) entre el primero y el ultimo de la fila o columna
                                     (append (swapV (first pixeles) (first (reverse pixeles))) (flip (reverse (cdr (reverse (cdr pixeles))))))
                                     )
                                 )
                    )
                
                  ;se define una funcion auxiliar donde entran los pixeles y la columna donde se trabajará
                  (define AUX (lambda (pixeles posY)
                                ;condición de borde, si la columna analizada es el ancho de la imagen:
                                (if (= posY (getWidthImage imagen))
                                    ;retorna null y procede a generar la salida
                                    null
                                    ;sino, realiza la recursión llamando a otra funcion auxiliar (flip) que logra invertir los pixeles, entregándole a esta los correspondientes a la fila analizada (filter) y el tipo de flip (int 0|1),
                                    ;agregando esto al llamado a sí misma generando la recursión natural
                                    (append (flip (filter (lambda (pixel) (= (getYposition pixel) posY)) pixeles)) (AUX pixeles (+ posY 1))) 
                                    )
                                 )
                    )
                ;se genera la salida con la funcion constructora de imagenes haciendo el llamado a la auxiliar ordenando previamente los pixeles que salen de esta mediante la funcion sort
                (image (getWidthImage imagen) (getHeightImage imagen) (sort (sort (AUX (getImagePixels imagen) 0) #:key getHeightImage <) #:key getWidthImage <))
               )
  )

;Descripción: funcion que corta un cuadrante de una imagen entrante
;Dominio: imagen, x1 (int), y1 (int), x2 (int), y2 (int) 
;Recorrido: imagen
;Recursión: no aplica
(define crop (lambda (imagen x1 y1 x2 y2)
               ;genera una imagen con las medidas del cuadrante ingresado donde sólo estaran los pixeles cuyas posiciones en X e Y calcen en lo solicitado
                (image (+ (- y2 y1) 1) (+ (- x2 x1) 1) (filter (lambda (pixel)
                     (and (and (>= (getXposition pixel) x1 ) (<= (getXposition pixel) x2 )) (and (>= (getYposition pixel) y1 ) (<= (getYposition pixel) y2)))
                         ) (getImagePixels imagen)))
               ))

;Descripción: funcion que transforma una imagen desde una representacion RGB a una representacion Hexadecimal donde cada valor rgb es representado por 2 caracteres en la salida hexadecimal, respetando el orden de colores de izq a derecha (R G B)
;Dominio: imagen 
;Recorrido: imagen
;Recursión: no aplica
(define imgRGB->imgHex (lambda (imagen)
                         ;genera la salida de la funcion principal, una imagen que obtiene los pixeles a partir de la funcion auxiliar (RGBtoHEX)
                         (image (getWidthImage imagen) (getHeightImage imagen)(map (lambda (pixelrgb) (pixhex-d (getXposition pixelrgb) (getYposition pixelrgb) (pixrgb->string pixelrgb) (getDepth pixelrgb))) (getImagePixels imagen)))
                         )
  )

;Descripción: funcion que permite obtener la frecuencia de colores a partir de la imagen entrante
;Dominio: imagen
;Recorrido: frecuencia de colores (list (frecuencia (int) color (bit hex rgb))
;Recursión: natural
(define histogram (lambda (image)
                  (define AUX (lambda (pixeles)
                                ;condicion de borde: si la lista de pixeles entrante es vacia
                                (if (null? pixeles)
                                    ;retorna null y arma la salida
                                    null
                                    ;descomposicion recursiva: genera una lista mediante recursion natural donde analiza el primer pixel, guardando la frecuencia y su color, siguiendo con el resto de pixeles
                                    ;eliminando los ya analizados
                                    (append (list (list (length (filter (lambda (pixel) (equal? (getPixelValue pixel) (getPixelValue (first pixeles)))) pixeles)) (getPixelValue (first pixeles))) ) (AUX (filter (lambda (pixel) (not (equal? (getPixelValue pixel) (getPixelValue (first pixeles))))) pixeles))))))
                  (AUX (getImagePixels image))
                    )
  )

;Descripción: funcion que rota la imagen entrante 90° a la derecha
;Dominio: imagen
;Recorrido: imagen
;Recursión: natural
(define rotate90 (lambda (imagen)
                   ;se define una funcion auxiliar con la que generar la recursion
                   (define AUX (lambda (pixeles typePixel)
                                   ;condicion de borde: si no quedan pixeles por analizar
                                   (if (null? pixeles)
                                       ;retorna null y comienza a formar la salida
                                       null
                                       ;sino, genera el llamado recursivo dejando un estado pendiente,
                                       ;agregando un pixel generado segun el tipo que corresponda mediante el analisis del alto de la imagen e intercambiando su posicion en Y por la posicion en X
                                       (append (list (if (equal? typePixel pixrgb-d)
                                                         (typePixel (getYposition (first pixeles)) (- (- (getHeightImage imagen) 1) (getXposition (first pixeles))) (getR (first pixeles)) (getG (first pixeles)) (getB (first pixeles)) (getDepth (first pixeles)))
                                                         (typePixel (getYposition (first pixeles)) (- (- (getHeightImage imagen) 1) (getXposition (first pixeles))) (getPixelValue (first pixeles)) (getDepth (first pixeles)))
                                                      ) )
                                               (AUX (cdr pixeles) typePixel))
                                       )))

                   ;realiza el llamado a la funcion auxiliar, generando la recursion
                   (image (getWidthImage imagen) (getHeightImage imagen) (sort (sort (AUX (getImagePixels imagen) (cond
                                                                                                                    ((bitmap? imagen) pixbit-d)
                                                                                                                    ((hexmap? imagen) pixhex-d)
                                                                                                                    ((pixmap? imagen) pixrgb-d))) #:key getHeightImage <) #:key getWidthImage <))
                   )
  )

;Descripción: funcion que comprime la imagen entrando eliminando el color mas frecuente
;Dominio: imagen
;Recorrido: imagen
;Recursión: no aplica
(define compress (lambda (imagen)
                      ;genera una imagen donde se intercambian los pixeles con mayor frecuencia por una estructura que aporta informacion para la proxima descompresion
                      (image (getWidthImage imagen) (getHeightImage imagen) (append (filter (lambda (pixel) (not (equal? (getPixelValue pixel) (getPixelValue (first (getImagePixels imagen)))))) (getImagePixels imagen)) (list (first(sort (histogram imagen) #:key car >))) ) )))


;Descripción: funcion que permite aplicar funciones especiales a la imagen entrante
;Dominio: funcion filtro, imagen
;Recorrido: imagen
;Recursión: no aplica
(define edit (lambda (f imagen) (image (getWidthImage imagen) (getHeightImage imagen) (map f (getImagePixels imagen)))))


;Descripción: funcion que intercambia el valor del bit del pixel entrante
;Dominio: pixbit-d
;Recorrido: pixbit-d
;Recursión: no aplica
(define invertColorBit (lambda (pixbit) (pixbit-d (getXposition pixbit) (getYposition pixbit) (if (= (getPixelValue pixbit) 0) 1 0) (getDepth pixbit))))

;Descripción: funcion que cambia simetricamente el valor del pixel rgb
;Dominio: pixrgb-d
;Recorrido: pixrgb-d
;Recursión: no aplica
(define invertColorRGB (lambda (pixrgb) (pixrgb-d (getXposition pixrgb) (getYposition pixrgb) (- 255 (getR pixrgb)) (- 255 (getG pixrgb)) (- 255 (getB pixrgb)) (getDepth pixrgb))))

;Descripción: funcion que permite ajustar cualquier canal de la imagen rgb entrante
;Dominio: funcion selectora, funcion modificadora, funcion operadora, pixrgb-d
;Recorrido: pixrgb-d
;Recursión: no aplica
(define adjustChannel (lambda (fSelec fMod fOp) (lambda (pixelRGB) (fMod pixelRGB (fOp (fSelec pixelRGB))))))



;Descripción: funcion que transforma una imagen a una representacion string
;Dominio: imagen, typeConvert (pixbit->string | pixhex->string | pixrgb->string)
;Recorrido: string
;Recursión: natural
;Motivo recursion: la recursion natural permite concatenar los strings a medida que se van obteniendo los valores de cada pixel
(define image->string (lambda (imagen typeC)
                        ;se define una funcion auxiliar con la que generar la recursion natural
                        (define AUX (lambda (pixeles typeC W)
                                      ;condicion de borde: si no quedan pixeles por analizar
                                      (if (null? pixeles)
                                          ;retorna un salto de linea para comenzar a armar la salida
                                          "\n"
                                          ;si el pixel anallizado es el ultimo de la fila
                                          ;descomposicion recursiva: obtiene el string del valor del pixel y genera el llamado de si mismo con el resto de pixeles
                                          (if (= (getYposition (first pixeles)) (- W 1))
                                              ;si el pixel es el ultimo de la fila agrega un salto de linea
                                              (string-append (typeC (first pixeles)) "\n" (AUX (cdr pixeles) typeC W))
                                              ;sino, agrega un espacio para diferenciar
                                              (string-append (typeC (first pixeles)) " " (AUX (cdr pixeles) typeC W))))))
                        ;realiza el llamado a la funcion auxiliar entregando los pixeles ordenados, el tipo de conversion y el ancho de la imagen
                        (AUX (sort (sort (getImagePixels imagen) #:key getHeightImage <) #:key getWidthImage <) typeC (getWidthImage imagen))
                        )
                        
  )