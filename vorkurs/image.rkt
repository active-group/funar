#lang deinprogramm/sdp/beginner
(require deinprogramm/sdp/image)
  
(define x
  (+ 23
     (* 2 42)))
(define y (+ x 1))

(define circle1 (circle 50 "solid" "red"))
(define square1 (square 100 "outline" "blue"))
(define star1 (star 50 "solid" "green"))

(define overlay1 (overlay star1 circle1))

#;(above
 (beside circle1 star1)
 (beside star1 circle1))

#;(above
 (beside square1 circle1)
 (beside circle1 square1))

; Abstraktion
; 2 ähnliche Codestellen

; - kopieren (ein letztes Mal)
; - Unterschiede durch (abstrakte) Namen
; - Namen in ein lambda aufnehmen

; Konstruktionsanleitung / design recipes

; Kurzbeschreibung:
; quadratisches Kachelmuster aus zwei Bildern berechnen

; Signatur
(: tile (image image -> image))

(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))

(tile circle1 star1)


#|
class C {
  static int m(int x) {
     ... x ...
     x = x + 1;
     ... x ...
  }

  ... C.m(42) ... -> { ... 42 ... }
}




|#