#lang deinprogramm/sdp/beginner

(require deinprogramm/sdp/image)
        
(define x
  (+ 23
     (* 42 2)))
(define y (* x 12))

(define circle1 (circle 50 "solid" "red"))
(define star1 (star "solid" "green"))
(define square1 (square "outline" "blue"))