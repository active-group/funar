#lang deinprogramm/sdp/beginner
(require deinprogramm/sdp/image)
(define x
  (+ 12
     (* 2 23)))
(define y (+ x 42))

; Kommentar

(define circle1 (circle 50 "solid" "red"))
(define square1 (square 100 "outline" "blue"))
(define star1 (star 50 "solid" "green"))
(define overlay1 (overlay star1 circle1))

(above
 (beside circle1 square1)
 (beside square1 circle1))

(above
 (beside star1 circle1)
 (beside circle1 star1))

; Conor McBride: No proliferation without abstraction.

; Abstrahieren (2 ähnliche Codestücke)
; - (ein letztes Mal) kopieren
; - Unterschiede durch (abstrakte) Namen ersetzen
; - Namen aufnehmen in lambda

(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))

(tile square1 star1)

(define double
  (lambda (x)
    (* x 2)))

(define mystery
  (lambda (y)
    (+ x y)))

; Lambda-Kalkül

; lexikalische / statische Bindung:
; Vorkommen einer Variable: von innen nach außen suchen
; erste Bindung (lambda oder define) ist zuständig

#|
// x steht für Speicherzelle, in der ein int steht
class C {
   static int m(int x) {
      ... x ...
      x = x + 1;
      ... x ...
   }
   ... C.m(42) ....
}


|#