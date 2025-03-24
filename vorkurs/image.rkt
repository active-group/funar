#lang deinprogramm/sdp/beginner
(require deinprogramm/sdp/image)

(define x
  (+ 42
     (* 23
        2)))

(define y (* x 2))

(define circle1 (circle 50 "solid" "red"))
(define square1 (square 100 "outline" "magenta"))
(define star1 (star 50 "solid" "gold"))

(define overlay1 (overlay star1 circle1))

#;(above
 (beside circle1 star1)
 (beside star1 circle1))

#;(above
 (beside square1 circle1)
 (beside circle1 square1))

; Abstraktion:
; - letztes Mal kopieren
; - Unterschiede durch abstrakte Namen ersetzen
; - Namen in lambda-Ausdruck aufnehmen

(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))

(tile star1 circle1)





