#lang deinprogramm/sdp
(require deinprogramm/sdp/image)
  
(define x
  (+ 12
     (* 4 8)))

(define circle1
  (circle 50 "solid" "red"))

(define square1
  (square 100 "outline" "gold"))

(define star1
  (star 50 "solid" "green"))

(define overlay1
  (overlay star1 circle1))

#;(above
 (beside circle1 star1)
 (beside star1 circle1))

#;(above
 (beside square1 circle1)
 (beside circle1 square1))

; Abstraktion
; - kopieren (letztes Mal)
; - Unterschiede durch (abstrakte) Namen ersetzen
; - lambda

; Konstruktionsanleitungen / design recipes

; Kurzbeschreibung:
; quadratisches Kachelmuster aus zwei Bildern herstellen
(: tile (image image -> image)) ; Signatur

; Beispiele/Testfälle

(check-expect (tile square1 circle1)
              (above (beside square1 circle1)
                     (beside circle1 square1)))

(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))

; (tile circle1 star1)

#|
class C {
   static int m(int x) {
     ... x ...
     x = x+1;
     ... x ...
   }
}

... C.m(42) ... ->
{ ... }

|#