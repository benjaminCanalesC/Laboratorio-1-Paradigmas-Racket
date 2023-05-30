#lang racket

(require "pixel.rkt")
(require "pixbit-d.rkt")
(require "pixhex-d.rkt")
(require "pixrgb-d.rkt")
(require "imagen.rkt")
(require "funcionalidades.rkt")

(define img1 (image 2 2
                  (pixrgb-d 0 0 255 0 0 10)
                  (pixrgb-d 0 1 0 255 0 20)
                  (pixrgb-d 1 0 0 0 255 30)
                  (pixrgb-d 1 1 255 255 255 40)
 ))


(define img2 (image 2 2
                  (pixbit-d 0 0 0 10)
                  (pixbit-d 0 1 1 20)
                  (pixbit-d 1 0 1 30)
                  (pixbit-d 1 1 0 255))
 )


(define img3 (imgRGB->imgHex img1))

(define img4 (image 3 2
                  (pixbit-d 0 0 0 10)
                  (pixbit-d 0 1 1 20)
		  (pixbit-d 0 2 0 30)
                  (pixbit-d 1 0 1 40)
                  (pixbit-d 1 1 0 50)
		  (pixbit-d 1 2 1 60))
 )

(define img5 (image 2 3
                  (pixrgb-d 0 0 255 255 255 10)
                  (pixrgb-d 0 1 23 200 12 20)
		  (pixrgb-d 1 0 13 255 20 30)
                  (pixrgb-d 1 1 23 200 12 40)
                  (pixrgb-d 2 0 130 25 45 50)
		  (pixrgb-d 2 1 23 200 12 60)
 ))



(define img6 (imgRGB->imgHex img5))

(display "bitmap?\n")
(bitmap? img1) ; la respuesta debería ser #f
(bitmap? img2)  ; la respuesta debería ser #t
(bitmap? img3)  ; la respuesta debería ser #f
(bitmap? img4) ; la respuesta debería ser #t
(bitmap? img5)  ; la respuesta debería ser #f
(bitmap? img6)  ; la respuesta debería ser #f

(display "\npixmap?\n")
(pixmap? img1) ; la respuesta debería ser #t
(pixmap? img2)  ; la respuesta debería ser #f
(pixmap? img3)  ; la respuesta debería ser #f
(pixmap? img4) ; la respuesta debería ser #f
(pixmap? img5)  ; la respuesta debería ser #t
(pixmap? img6)  ; la respuesta debería ser #f


(display "\nhexmap?\n")
(hexmap? img1) ; la respuesta debería ser #f
(hexmap? img2)  ; la respuesta debería ser #f
(hexmap? img3)  ; la respuesta debería ser #t
(hexmap? img4) ; la respuesta debería ser #f
(hexmap? img5)  ; la respuesta debería ser #f
(hexmap? img6)  ; la respuesta debería ser #t


(display "\ncompressed?\n")
(compressed? img1) ; la respuesta debería ser #f
(compressed? img2) ; la respuesta debería ser #f
(compressed? img3) ; la respuesta debería ser #f
(compressed? img4) ; la respuesta debería ser #f
(compressed? img5) ; la respuesta debería ser #f
(compressed? img6) ; la respuesta debería ser #f


(display "\nflipH\n")
(flipH img1)
(flipH img2)
(flipH img3)
(flipH img4)
(flipH img5)
(flipH img6)


(display "\nflipV\n")
(flipV img1)
(flipV img2)
(flipV img3)
(flipV img4)
(flipV img5)
(flipV img6)

(define img7 (crop img1 0 0 0 0)) ; debería retornar una imágen con un pixel
(define img8 (crop img2 0 0 0 1)) ; debería retornar una imágen con dos pixeles
(define img9 (crop img1 0 1 1 1)) ; debería retornar una imágen con dos pixeles
(define img10 (crop img2 0 0 1 1)) ; debería retornar la misma imagen
(define img11 (crop img4 0 0 0 1))
(define img12 (crop img5 0 0 0 0))
(define img13 (crop img4 0 1 1 1))                                             


(display "\nhistogram\n")
(histogram img1)
(histogram img2)
(histogram img3)
(histogram img4)
(histogram img5)
(histogram img6)
(histogram img7)
(histogram img8)
(histogram img9)
(histogram img10)
(histogram img11)
(histogram img12)
(histogram img13)

(define img14 (rotate90 img1))
(define img15 (rotate90 img2))
(define img16 (rotate90 img3))
(define img17 (rotate90 img4))
(define img18 (rotate90 img5))
(define img19 (rotate90 img6))
(define img20 (rotate90 img7))
(define img21 (rotate90 img8))
(define img22 (rotate90 img9))
(define img23 (rotate90 img10))
(define img24 (rotate90 img11))
(define img25 (rotate90 img12))
(define img26 (rotate90 img13))

(define img27 (compress img1))
(define img28 (compress img2))
(define img29 (compress img3))
(define img30 (compress img4))
(define img31 (compress img5))
(define img32 (compress img6))
(define img33 (compress img7))
(define img34 (compress img8))
(define img35 (compress img9))
(define img36 (compress img10))
(define img37 (compress img11))
(define img38 (compress img12))
(define img39 (compress img13))

(display "\ncompressed?\n")
(compressed? img27)  ; la respuesta debería ser #t
(compressed? img28)  ; la respuesta debería ser #t
(compressed? img29)  ; la respuesta debería ser #t
(compressed? img30)  ; la respuesta debería ser #t
(compressed? img31)  ; la respuesta debería ser #t
(compressed? img32)  ; la respuesta debería ser #t
(compressed? img33)  ; la respuesta debería ser #t
(compressed? img34)  ; la respuesta debería ser #t
(compressed? img35)  ; la respuesta debería ser #t
(compressed? img36)  ; la respuesta debería ser #t
(compressed? img37)  ; la respuesta debería ser #t
(compressed? img38)  ; la respuesta debería ser #t
(compressed? img39)  ; la respuesta debería ser #t

(define img40 (edit invertColorBit img2))
(define img41 (edit invertColorRGB img1))

(define img42 (edit invertColorBit img4))
(define img43 (edit invertColorRGB img5))

(define img44 (edit (adjustChannel getR setR incCh) img1))
(define img45 (edit (adjustChannel getG setG incCh) img1))
(define img46 (edit (adjustChannel getB setB incCh) img1))
(define img47 (edit (adjustChannel getR setR incCh) img5))
(define img48 (edit (adjustChannel getG setG incCh) img5))
(define img49 (edit (adjustChannel getB setB incCh) img5))

(display "\nimage->string\n")
;imágenes no comprimidas
(display (image->string img1 pixrgb->string))
(display (image->string img2 pixbit->string))
(display (image->string img3 pixhex->string))
(display (image->string img4 pixbit->string))
(display (image->string img5 pixrgb->string))
(display (image->string img6 pixhex->string))
(display (image->string img7 pixrgb->string))
(display (image->string img8 pixbit->string))
(display (image->string img9 pixrgb->string))
(display (image->string img10 pixbit->string))
(display (image->string img11 pixbit->string))
(display (image->string img12 pixrgb->string))
(display (image->string img13 pixbit->string))