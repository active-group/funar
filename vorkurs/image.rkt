#lang deinprogramm/sdp/beginner
(require teachpack/deinprogramm/sdp/image)

(define x
  (+ 12
     (* 42 3)))

(define circle1 (circle 50 "solid" "red"))
(define square1 (square 100 "outline" "blue"))
(define star1 (star 50 "solid" "gold"))

(define overlay1 (overlay star1 circle1))

#;(above
 (beside circle1 star1)
 (beside star1 circle1))

#;(above
 (beside square1 circle1)
 (beside circle1 square1))

; Abstraktion

; Kurzbeschreibung:
; Kachelmuster aus zwei Bildern zusammensetzen 

; Signatur
(: tile (image image -> image))

; Test

(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))

;(tile square1 circle1)

#|
class C {
   static void m(int x) {
     // x steht für eine Speicherzelle, in der eine Zahl drin ist
     ... x ...
     x = x + 1;
     ... x ...
   }

  C.m(17) -> {
     ... 17 ...
  }
}

|#

#|

class K {
  public int x;
  K(int x) { this.x = x }
}

m(K k) {
  k.x = k.x +1;
}

|#

