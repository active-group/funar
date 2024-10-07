#lang deinprogramm/sdp/beginner
(require deinprogramm/sdp/image)

(define x (+ 23 42))
(define y
  (+ 23
     (* 42 2)))

; Kommentar

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
; Voraussetzung: 2 ähnliche Code-Stellen
; - kopieren (ein letztes Mal)
; - Unterschiede durch abstrakte Namen
; - lambda mit den Namen als Parameter

; Konstruktionsanleitung
; - Kurzbeschreibung


(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))

;(tile star1 circle1)

#|
class C {
   static int m(int x) {
     // x steht für eine Speicherzelle, in der eine Zahl steht
     ... x ...
     x = x + 1;
     ... x ...
   }
   ... C.m(42) -> { ... 42 ... }
}
|#