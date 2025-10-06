#lang deinprogramm/sdp/beginner

(require deinprogramm/sdp/image)
        
(define x
  (+ 23
     (* 42 2)))
(define y (* x 12))

(define circle1 (circle 50 "solid" "red"))
(define star1 (star 50 "solid" "green"))
(define square1 (square 100 "outline" "blue"))
(define overlay1 (overlay star1 circle1))


(above
 (beside star1 circle1)
 (beside circle1 star1))

(above
 (beside circle1 square1)
 (beside square1 circle1))

; Abstraktion - 2 ähnliche Code-Stellen
; - noch einmal kopieren
; - Unterschiede durch abstrakte Namen ersetzen
; - Namen in lambda

(lambda (image1 image2)
  (above
   (beside image1 image2)
   (beside image2 image1)))
