#lang deinprogramm/sdp/beginner
(require deinprogramm/sdp/image)
(define x (+ 23 42))
(define y
  (+ x
     (* 5
        12)))

(define circle1 (circle 50 "solid" "blue"))
(define square1 (square 100 "outline" "gold"))
(define star1 (star 50 "solid" "green"))