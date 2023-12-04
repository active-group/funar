#lang deinprogramm/sdp/beginner
(require deinprogramm/sdp/image)
(define x
  (+ 12
     (* 23
        4)))

(define y (- x 7))

(define circle1 (circle 50 "solid" "red"))
(define star1 (star 50 "solid" "gold"))
(define square1 (square 100 "outline" "blue"))
(define overlay1 (overlay star1 circle1))

#;(above
 (beside circle1 star1)
 (beside star1 circle1))

#;(above
 (beside square1 circle1)
 (beside circle1 square1))

; abstrahieren:
; - noch einmal kopieren
; - für Unterschiede abstrakte Namen
; - Namen -> Parameter in lambda-Ausdruck
(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))