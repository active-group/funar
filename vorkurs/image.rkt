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
(define overlay1 (overlay star1 circle1))

(above
 (beside circle1 square1)
 (beside square1 circle1))

(above
 (beside star1 circle1)
 (beside circle1 star1))

(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))