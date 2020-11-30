;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname images) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
; Vorkurs
; Menü: Language/Sprache -> Choose Language
; Schreibe Dein Programm! - Anfänger
; Language -> Add Teachpack -> image.rkt -> OK
(define x 23)
(define y
  (+ 1
     (* 23 42)))

(define star1 (star 50 "solid" "green"))
(define circle1 (circle 50 "outline" "red"))
(define square1 (square 100 "solid" "gold"))

#;(above
 (beside star1 circle1)
 (beside circle1 star1))

#;(above
 (beside square1 star1)
 (beside star1 square1))

; Kurzbeschreibung
; Quadratisches Kachelmuster erzeugen
; Signatur
(: tile (image image -> image))

; Beispiele/Tests
; (grafischer Test weggelassen)

(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))

;(tile star1 circle1)

#|

Variable steht für eine Speicherzelle,
in der ein Wert steht.

class C {
  m(int x) {
    x = 10;
    ... x ...;
    x = 7;
    ... x ...;
  }
  ... m(5) ...
}

|#
