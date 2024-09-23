#lang deinprogramm/sdp/beginner
(require deinprogramm/sdp/image)
(define x
  (+ 12
     (* 23
        14)))
(define y (* x 2))

(define circle1 (circle 50 "solid" "red"))
(define square1 (square 100 "outline" "blue"))
(define star1 (star 50 "solid" "gold"))

(above
 (beside star1 circle1)
 (beside circle1 star1))
