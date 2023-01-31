#lang deinprogramm/sdp/beginner
(require deinprogramm/sdp/image)

  
(define x
  (+ 12
     (* 23
        35)))
(define y (* x 2))
(define z (+ x y))

(define circle1 (circle 50 "solid" "red"))
(define square1 (square 100 "outline" "green"))
(define star1 (star 50 "solid" "gold"))
(define overlay1 (overlay star1 circle1))

; Zwei ähnliche Code-Fragmente:
(above
 (beside circle1 star1)
 (beside star1 circle1))

(above
 (beside star1 square1)
 (beside square1 star1))

; Abstraktion:
; 1. kopieren
; 2. Unterschiede ersetzen wir durch ("abstrakte") Namen
; 3. lambda -> Funktion

(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))