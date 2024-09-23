#lang deinprogramm/sdp/beginner
(require deinprogramm/sdp/image)
(define x
  (+ 12
     (* 23
        14)))
(define y (* x 2))

(define circle1 (circle 50 "solid" "red"))
(define square1 (square 100 "outline" "blue"))
(define star1 (star 50 "solid" "gold"))

#;(above
 (beside star1 circle1)
 (beside circle1 star1))

#;(above
 (beside circle1 square1)
 (beside square1 circle1))

; abstrahieren
; 2 ähnliche Code-Passagen
; - (einmal noch) kopieren
; - Unterschiede durch (abstrakte) Namen
; - Namen in Lambda-Ausdruck aufgenommen

; Quadratisches Kachelmuster aus 2 Bildern zusammensetzen

; Signatur:
(: tile (image image -> image))

(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))

;(tile star1 circle1)

#|
"Referenz"

int m(int x) { // x steht für Referenz/Speicherzelle, in der Zahl steht
  x
  ...
  x = x + 1;
  x
}

m(42) -> { 42 ... 42 }
|#