#lang deinprogramm/sdp/beginner
(require deinprogramm/sdp/image)
(define x
  (+ 23
     (* 42 2)))
(define y (* x 2))

(define circle1 (circle 50 "solid" "green"))
(define square1 (square 100 "outline" "blue"))
(define star1 (star 50 "solid" "gold"))
(define overlay1 (overlay star1 circle1))

#;(above
 (beside circle1 star1)
 (beside star1 circle1))

#;(above
 (beside star1 square1)
 (beside square1 star1))

; Abstraktion
; 2 ähnliche Code-Stücke
; - (ein letztes Mal) kopieren
; - die Unterschiede ersetzen durch (abstrakte) Namen
; - Namen in ein lambda aufnehmen

; Konstruktionsanleitung
; - Kurzbeschreibung
; - Signatur(deklaration)
; - Beispiele/Tests
; - Definition

; quadratisches Kachelmuster erzeugen
(: tile (image image -> image))

(check-expect (tile star1 circle1)
              (above
               (beside star1 circle1)
               (beside circle1 star1)))

(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))

;(tile star1 square1)

#|

class C {
   static int f(int x) {
      ... x ...
      x = x + 1;
      ... x ...
   }
}

... C.f(42) ... ->
{
  ... 42 ...

  ... 43 ...
}


|#
