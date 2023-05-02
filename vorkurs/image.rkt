#lang deinprogramm/sdp/beginner
(require deinprogramm/sdp/image)

(define x
  (+ 12
     (* 23
        45)))

(define circle1 (circle 50 "solid" "yellow"))
(define square1 (square 100 "outline" "red"))
(define star1 (star 50 "solid" "blue"))
(define overlay1 (overlay star1 circle1))

#;(above
 (beside circle1 star1)
 (beside star1 circle1))

#;(above
 (beside square1 circle1)
 (beside circle1 square1))

; Abstraktion
; - kopieren (letztes Mal)
; - den Unterschieden Namen geben
; - über den Namen mit lambda abstrahieren -> Funktion

(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))