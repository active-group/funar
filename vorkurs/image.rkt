#lang deinprogramm/sdp/beginner
(require deinprogramm/sdp/image)
(define x
  (* 12
     (* 6
        5)))

(define y
  (+ 12
     4 5 6))

(define circle1 (circle 50 "solid" "gold"))
(define square1 (square 100 "outline" "green"))
(define star1 (star 50 "solid" "blue"))
(define overlay1 (overlay star1 circle1))

#;(above
 (beside circle1 star1)
 (beside star1 circle1))

#;(above
 (beside star1 square1)
 (beside square1 star1))

; Abstraktion:
; 1. letztes Mal kopieren
; 2. Unterschieden Namen geben
; 3. lambda

; Konstruktionsanleitungen
; - Kurzbeschreibung
; - Signatur(deklaration)

; Quadratisches Kachelmuster aus zwei Bildern

(: tile (image image -> image))

(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))

; (z (image1 image2))
(define image1 (rectangle 200 100 "solid" "yellow"))

(tile square1 star1)