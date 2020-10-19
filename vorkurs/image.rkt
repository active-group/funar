;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname image) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
; Abelson/Sussman/Sussman
; Structure and Interpretation of Computer Programs
; https://mitpress.mit.edu/sites/default/files/sicp/index.html

(define x
  (* 12
     (+ 23
        42)))

(define circle1 (circle 50 "solid" "red"))
(define square1 (square 100 "outline" "blue"))
(define star1 (star 50 "solid" "gold"))
(define beside1 (beside circle1 square1))

#;(above
 (beside star1 circle1)
 (beside circle1 star1))

#;(above
 (beside square1 star1)
 (beside star1 square1))

; Konstruktionsanleitungen
; Feste Schrittfolge

; Kurzbeschreibung
; Quadratisches Kachelmuster bilden
; Signatur
(: tile (image image -> image))

(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))

#;(tile circle1 star1)

#|

void m(...) {
  int x = 780;

  x = 781;

  ... x ...
}

|#