;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname intro) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
(define x
  (+ 12
     (* 23
        42)))

(define circle1 (circle 50 "solid" "red"))
(define square1 (square 100 "outline" "blue"))
(define star1 (star 50 "solid" "green"))
(define overlay1 (overlay star1 square1))
(define overlay2 (overlay overlay1 circle1))

; Zeilenkommentar

#;(above
 (beside circle1 star1)
 (beside star1 circle1))

#;(above
 (beside star1 square1)
 (beside square1 star1))

; 1. Schritt: Kurzbeschreibung
; Quadratisches Kachelmuster herstellen
; 2. Schritt: Signaturdeklaration
(: tile (image image -> image))


(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))

;(tile star1 circle1)

#|
class C {

  static int m(int x) {
     // x ist der Name für eine Speicherzelle
     x = 5;
     ... x ...
  }

  static int m2(final C2 x) {


     x.die();

     ... x ...
  }

  ... C.m(17) ...
}
|#