#lang deinprogramm/sdp/beginner
(require deinprogramm/sdp/image)

(define x
  (+ 12
     (* 42
        12)))

(define y (* x 15))

(define circle1 (circle 50 "solid" "red"))
(define square1 (square 100 "outline" "blue"))
(define star1 (star 50 "solid" "gold"))
(define overlay1 (overlay star1 circle1))

(above
 (beside star1 circle1)
 (beside circle1 star1))

(above
 (beside square1 star1)
 (beside star1 square1))

; Abstraktion
; 2 Beispiele
; Unterschiede -> Name
; lambda

; Konstruktionsanleitungen / design recipes

; Kurzbeschreibung
; quadratisches Kachelmuster aus 2 Bildern

; Signatur
(: tile (image image -> image))


(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))