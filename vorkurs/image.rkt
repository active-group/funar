#lang deinprogramm/sdp/beginner
(require deinprogramm/sdp/image)
(define x
  (+ 23
     (* 42 2)))
(define y (* x 2))

(define circle1 (circle 50 "solid" "green"))
(define square1 (square 100 "outline" "blue"))
(define star1 (star 50 "solid" "gold"))