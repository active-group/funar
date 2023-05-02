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

(above
 (beside circle1 star1)
 (beside star1 circle1))