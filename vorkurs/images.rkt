#lang deinprogramm/sdp/beginner
(require deinprogramm/sdp/image)

(define x (+ 23 42))
(define y
  (+ 23
     (* 42 2)))

; Kommentar

(define circle1 (circle 50 "solid" "red"))
(define square1 (square 100 "outline" "blue"))
(define star1 (star 50 "solid" "green"))
(define overlay1 (overlay star1 circle1))