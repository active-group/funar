#lang deinprogramm/sdp/beginner
(require deinprogramm/sdp/image)

(define x
  (+ 13
     (* 2
        23)))
(define y
  (* x 2))

(define circle1 (circle 50 "solid" "red"))
(define square1 (square 100 "outline" "blue"))
(define star1 (star 50 "solid" "green"))
  
(define overlay1 (overlay star1 circle1))

(above
 (beside star1 circle1)
 (beside circle1 star1))

(above
 (beside square1 star1)
 (beside star1 square1))

; Abstraction
; 2 similar/related pieces of code
; - copy (one last time)
; - replace differences by (abstract) names
; - put the names in a lambda -> function

; short description:
; create square tile pattern from 2 images

; signature
(: tile (image image -> image))

(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))