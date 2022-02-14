#lang deinprogramm/sdp/beginner
(require teachpack/deinprogramm/sdp/image)

(define x
  (+ 12
     (* 23 5)))

(define circle1 (circle 50 "solid" "red"))
(define star1 (star 50 "solid" "green"))
(define square1 (square 100 "outline" "blue"))
(define overlay1 (overlay star1 circle1))

(above
 (beside circle1 star1)
 (beside star1 circle1))

(above
 (beside star1 square1)
 (beside square1 star1))

(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))
