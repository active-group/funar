#lang deinprogramm/sdp/beginner
(require deinprogramm/sdp/image)
(define x
  (+ 12
     (* 23
        4)))

(define y (- x 7))

(define circle1 (circle 50 "solid" "red"))
(define star1 (star 50 "solid" "gold"))
(define square1 (square 100 "outline" "blue"))
(define overlay1 (overlay star1 circle1))

#;(above
 (beside circle1 star1)
 (beside star1 circle1))

#;(above
 (beside square1 circle1)
 (beside circle1 square1))

; abstrahieren:
; - noch einmal kopieren
; - für Unterschiede abstrakte Namen
; - Namen -> Parameter in lambda-Ausdruck

; Konstruktionsanleitungen / design recipes

; Kurzbeschreibung
; quadratisches Kachelmuster erzeugen

; Signatur
(: tile (image image -> image))

; Beispiele/Tests
(check-expect (tile star1 circle1)
              (above (beside star1 circle1)
                     (beside circle1 star1)))

(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))

;(tile star1 circle1)

#|
class C {
   int y;
   int m(int x) {
      ... x ...
      x = x + 1;
      y = y + 1;
      f(z -> x ... y)
      ... x ...
   }
}

  C.m(42)
|#
