;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname intro) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))



(define x (+ 12 23))
(define y
  (* 12
     (* 23
        42)))

(define f
  (lambda (x)
    (+ x y)))

; Bindung zu einem Vorkommen einer Variable:
; - innen nach außen suchen
; - lambda mit Parameter
; - Definition
; - eingebaute / importierte Definition

(define circle1 (circle 50 "solid" "red"))
(define square1 (square 100 "outline" "green"))
(define star1 (star 50 "solid" "green"))
(define overlay1 (overlay star1 circle1))

; Zeilenkommentar

#;(above
 (beside circle1 star1)
 (beside star1 circle1))

#;(above
 (beside overlay1 circle1)
 (beside circle1 overlay1))

; Kurzbeschreibung
; Quadratisches Kachelmuster erzeugen

; Signaturdeklaration
(: tile (image image -> image))

; Tests
(check-expect (tile circle1 star1)
              (above
               (beside circle1 star1)
               (beside star1 circle1)))

(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))

; (tile circle1 star1)
;(tile "circle1" "star1")

#|
class C {
  static int f(int x) {
    x = x * 2;
    return x + 1;
  }

  ... f(42) ...
}

|#