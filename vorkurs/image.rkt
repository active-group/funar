#lang deinprogramm/sdp/beginner
(require deinprogramm/sdp/image)
  
(define x (+ 23 42))
(define y (* x 2))
(define z
  (+ 23
     (* 2
        42)))

(define circle1
  (circle 50 "solid" "red"))
(define square1
  (square 100 "outline" "blue"))
(define star1
  (star 50 "solid" "green"))
(define overlay1
  (overlay star1 circle1))

#;(above
 (beside circle1 star1)
 (beside star1 circle1))

#;(above
 (beside star1 square1)
 (beside square1 star1))

; Abstrahieren
; - 2 ähnliche Code-Passagen
; - (das letzte Mal) kopieren
; - Unterschiede durch ("abstrakte") Namen
; - lambda-Ausdruck mit Namen als Parameter
(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))

(tile square1 circle1)