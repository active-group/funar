#lang deinprogramm/sdp/beginner

; schreibedeinprogramm.de

(require deinprogramm/sdp/image)

(* 12 (/ 3 2))

(define circle1 (circle 50 "solid" "gold"))
(define square1 (square 100 "outline" "green"))
(define star1 (star 50 "solid" "blue"))
(define overlay1 (overlay star1 circle1))

(above
 (beside circle1 star1)
 (beside star1 circle1))

(above
 (beside star1 square1)
 (beside square1 star1))

; Abstraktion
; 1. noch ein mal kopieren
; 2. den Unterschieden namen geben
; 3. lambda

; Konstruktionsanleitung
; 1. Kurzbeschreibung schreiben
; 2. Signatur
; 3. Test

; Quadratisches Kachelmuster erzeugen
(: tile (image image -> image))

(check-expect (tile star1 square1)
              (above
               (beside star1 square1)
               (beside square1 star1)))

(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))