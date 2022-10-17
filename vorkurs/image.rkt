#lang deinprogramm/sdp/beginner
(require deinprogramm/sdp/image)

(define x
  (+ 23
     (* 42
        5)))

(define circle1 (circle 50 "solid" "green"))
(define square1 (square 100 "outline" "yellow"))
(define star1 (star 50 "solid" "blue"))
(define overlay1 (overlay star1 circle1))

(above
 (beside star1 circle1)
 (beside circle1 star1))

(above
 (beside square1 star1)
 (beside star1 square1))

; Abstraktion
; 1. kopieren
; 2. Unterschiede durch Namen ersetzen

(above
 (beside image1 image2)
 (beside image2 image1))
