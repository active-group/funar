#lang deinprogramm/sdp/beginner
(require deinprogramm/sdp/image)
(define x
  (* 12
     (* 6
        5)))

(define y
  (+ 12
     4 5 6))

(define circle1 (circle 50 "solid" "gold"))
(define square1 (square 100 "outline" "green"))
(define star1 (star 50 "solid" "blue"))
(define overlay1 (overlay star1 circle1))

                                     
  