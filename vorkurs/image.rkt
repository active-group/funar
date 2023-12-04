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

(above
 (beside circle1 star1)
 (beside star1 circle1))