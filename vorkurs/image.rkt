#lang deinprogramm/sdp/beginner
(require deinprogramm/sdp/image)

(define x
  (+ 23
     (* 42 5)))

(define circle1 (circle 50 "solid" "gold"))
(define square1 (square 100 "outline" "green"))
(define star1 (star 50 "solid" "blue"))
(define overlay1 (overlay star1 circle1))

#;(above
 (beside circle1 star1)
 (beside star1 circle1))

#;(above
 (beside square1 circle1)
 (beside circle1 square1))

; Konstruktionsanleitungen

; Schritt 1: Kurzbeschreibung
; quadratisches Kachelmuster erstellen

; Schritt 2: Signaturdeklaration
(: tile (image image -> image))

; Schritt 3: Tests
(check-expect (tile square1 overlay1)
              (above (beside square1 overlay1)
                     (beside overlay1 square1)))

; Funktionsdefinition
(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))