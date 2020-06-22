;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname intro) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
(define x
  (+ 12
     42))

(define circle1 (circle 50 "solid" "red"))
(define star1 (star 50 "outline" "blue"))

(define overlay1 (overlay star1 circle1))

(define square1 (square 100 "solid" "gold"))

; Zeilenkommentar

#;(above
 (beside circle1 star1)
 (beside star1 circle1))

#;(above
 (beside square1 circle1)
 (beside circle1 square1))


; Kachelmuster aus zwei Bildern machen
; Signaturdeklaration
(: tile (image image -> image))

(check-expect (tile circle1 star1)
              (above
               (beside circle1 star1)
               (beside star1 circle1)))

(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))

; Datendefinition:
; Ein Haustier ist eins der folgenden: <- Fallunterscheidung
; - Hund
; - Katze
; - Schlange
; Spezialfall: Aufzählung
(define pet
  (signature (enum "Hund" "Katze" "Schlange")))

; ...
; Ist ein Haustier niedlich?
(: cute? (pet -> boolean))

(check-expect (cute? "Hund") #t)
(check-expect (cute? "Katze") #t)
(check-expect (cute? "Schlange") #f)

; Gerüst
#;(define cute?
  (lambda (pet)
    ...))

#|
imperative Sprachen:
Variable steht für eine Speicherzelle, deren Inhalt kann sich ändern.

class C {
  ... m(int x) {
     y = x + 1;
     x = x - 1;
     z = x * 2;
  }

  ... m(5) ...
}

|#

(define cute?
  (lambda (pet)
    (cond
      ; Hund
      ((string=? pet "Hund") #t)
      ; Katze
      ((string=? pet "Katze") #t)
      ; Schlange
      ((string=? pet "Schlange") #f)
      (else (violation "unbekanntes Haustier")))))

; Signaturverletzung und Fehler
; (cute? "Fisch")

