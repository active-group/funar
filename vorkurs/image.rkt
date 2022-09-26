#lang deinprogramm/sdp/beginner
(require deinprogramm/sdp/image)
  
(define x
  (+ 42
     (* 5
        23)))

(define circle1 (circle 50 "solid" "red"))
(define square1 (square 100 "outline" "blue"))
(define star1 (star 50 "solid" "gold"))
(define overlay1 (overlay star1 circle1))

; Zeilenkommentar

#;(above
 (beside circle1 star1)
 (beside star1 circle1))

#;(above
 (beside star1 square1)
 (beside square1 star1))

(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))

; (tile star1 circle1)


#|
class C {
  static m(int x) {

     ... x ...
     x = x + 1;
     ... x ...
  }

  ... C.m(17) ... => wird x durch 17 ersetzt?

}

|#
