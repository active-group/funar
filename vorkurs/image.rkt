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