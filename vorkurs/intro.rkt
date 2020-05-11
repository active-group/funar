;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname intro) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
(define mike "sperber")
(define pi 3.14159265)

(define s1 (star 50 "solid" "red"))
(define r1 (rectangle 200 100 "outline" "yellow"))
(define c1 (circle 50 "solid" "gold"))

(define b1 (beside s1 c1))

; (beside b1 (above s1 r1))

#;(above ; ganzen Ausdruck auskommentieren
 (beside s1 c1)
 (beside c1 s1))

(define sq1 (square 100 "solid" "green"))

#;(above
 (beside c1 sq1)
 (beside sq1 c1))

; Kachelmuster aus zwei Bildern machen
; 2stellige Funktion, 2 Images rein, 1 Image raus
(: tile (image image -> image))

(check-expect (tile s1 c1) (above
                            (beside s1 c1)
                            (beside c1 s1)))

(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))

#|
class C {
  static T m(int x) {
     ... x ...
     ++x;
     ... x ...
  }
}

C.m(5);

|#

; Ein Haustier ist eins der folgenden:
; - Hund
; - Katze
; - Schlange
; Fallunterscheidung / speziell: Aufzählung
(define pet
  (signature (enum "Hund" "Katze" "Schlange")))
  
; Ist Haustier niedlich?
(: cute? (pet -> boolean))

(check-expect (cute? "Hund") #t)
(check-expect (cute? "Katze") #t)
(check-expect (cute? "Schlange") #f)

#;(define cute?
  (lambda (pet)
    ...)) ; Gerüst

(define cute?
  (lambda (pet)
    (cond ; 3 Fälle => 3 Zweige, Schablone
      ; Zweig: (Bedingung Ergebnis)
      ((string=? pet "Hund") #t)
      ((string=? pet "Katze") #t)
      ((string=? pet "Schlange") #f))))

; Tiere auf dem texanischen Highway

; Ein Gürteltier hat folgende Eigenschaften: ("besteht aus")
; - lebendig oder tot
; - Gewicht
; zusammengesetzte Daten
(define-record dillo ; Signatur
  make-dillo ; Konstruktor
  (dillo-alive? boolean) ; Selektor ("Getter")
  (dillo-weight rational))

(: make-dillo (boolean rational -> dillo))
(: dillo-alive? (dillo -> boolean))
(: dillo-weight (dillo -> rational))

; Signaturverletzung:
; (make-dillo 10 #t)

(define dillo1 (make-dillo #t 10)) ; lebendiges Gürteltier, 10kg schwer
(define dillo2 (make-dillo #f 12)) ; Gürteltier tot, 12kg schwer


#|
class Dillo {
  bool isAlive;
  double weight;
  void runOver() {
    this.isAlive = false;
  }
}
|#

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (dillo-alive? (run-over-dillo dillo1))
              #f)
(check-expect (dillo-weight (run-over-dillo dillo1))
              10)

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))
(check-expect (run-over-dillo dillo2)
              dillo2)

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))


              


