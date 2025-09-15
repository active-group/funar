#lang deinprogramm/sdp/beginner
(require deinprogramm/sdp/image)
; Definition
(define x
  (+ 42
     (* 2 23)))
(define y (* x 2))

(define circle1 (circle 50 "solid" "blue"))
(define square1 (square 100 "outline" "red"))
(define star1 (star 50 "solid" "green"))
(define overlay1 (overlay star1 circle1))

(above
 (beside circle1 star1)
 (beside star1 circle1))

(above
 (beside square1 circle1)
 (beside circle1 square1))

; Abstraktion
; Zwei ähnliche Stellen Code
; - noch einmal kopieren
; - Unterschiede durch abstrakte Namen ersetzen
(above
 (beside image1 image2)
 (beside image2 image1))

