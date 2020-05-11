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
  dillo?
  (dillo-alive? boolean) ; Selektor ("Getter")
  (dillo-weight rational))

(: make-dillo (boolean rational -> dillo))
(: dillo-alive? (dillo -> boolean))
(: dillo-weight (dillo -> rational))
(: dillo? (any -> boolean)) ; Prädikat

; Signaturverletzung:
; (make-dillo 10 #t)

(define dillo1 (make-dillo #t 10)) ; lebendiges Gürteltier, 10kg schwer
(define dillo2 (make-dillo #f 12)) ; Gürteltier tot, 12kg schwer


#|
interface Animal {
  void runOver();
  void feed();
}

class Dillo implements Animal {
  bool isAlive;
  double weight;
  @Override
  void runOver() {
    this.isAlive = false;
  }
}

class Parrot implements Animal {
  @Override
  void runOver() {
    ...
  }
}

class Snake implements Animal {
  ...
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

; Gürteltier füttern
(: feed-dillo (rational dillo -> dillo))

(check-expect (feed-dillo 1 dillo1)
              (make-dillo #t 11))

(check-expect (feed-dillo 5 dillo2)
              dillo2)

#;(define feed-dillo
  (lambda (amount dillo)
    (if (dillo-alive? dillo)
        (make-dillo (dillo-alive? dillo)
                    (+ (dillo-weight dillo)
                       amount))
        dillo)))

#;(define feed-dillo
  (lambda (amount dillo)
    (if (dillo-alive? dillo)
        (make-dillo (dillo-alive? dillo)
                    (+ (dillo-weight dillo)
                       amount))
        (make-dillo (dillo-alive? dillo)
                    (dillo-weight dillo)))))


(define feed-dillo
  (lambda (amount dillo)
    (make-dillo (dillo-alive? dillo)
                (if (dillo-alive? dillo)
                    (+ (dillo-weight dillo)
                       amount)
                    (dillo-weight dillo)))))

; Ein Papagei hat folgende Eigenschaften:
; - einen Satz
; - Gewicht
(define-record parrot
  make-parrot
  parrot?
  (parrot-sentence string)
  (parrot-weight rational))

(define parrot1 (make-parrot "Hallo!" 2)) ; netter Papagei, 2kg
(define parrot2 (make-parrot "Tschüss!" 1.5)) ; unfreundlicher Papagei, 1.5kg

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot1)
              (make-parrot "" 2))
(check-expect (run-over-parrot parrot2)
              (make-parrot "" 1.5))

(define run-over-parrot
  (lambda (parrot)
    (make-parrot "" (parrot-weight parrot))))

; Papagei füttern
(: feed-parrot (rational parrot -> parrot))

(check-expect (feed-parrot 3 parrot1)
              (make-parrot "Hallo!" 5))
(check-expect (feed-parrot 2 parrot2)
              (make-parrot "Tschüss!" 3.5))

(define feed-parrot
  (lambda (amount parrot)
    (make-parrot (parrot-sentence parrot)
                 (+ (parrot-weight parrot)
                    amount))))
    
; Ein Tier auf dem texanischen Highway ist eins folgenden:
; - ein Gürteltier
; - ein Papagei
; Fallunterscheidung, hier: gemischte Daten
(define animal
  (signature
   (mixed dillo parrot)))

; Tier überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal dillo1)
              (run-over-dillo dillo1))
(check-expect (run-over-animal parrot1)
              (run-over-parrot parrot1))

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (run-over-dillo animal))
      ((parrot? animal) (run-over-parrot animal)))))
    
; zusammengesetzte Daten "UND"
; Fallunterscheidung / gemischte Daten "ODER"

; Eine Liste ist eins der folgenden:
; - die leere Liste
; - eine Cons-Liste aus: erstes Element und Rest-Liste
;                                                ^^^^^ Selbstbezug

(define list-of-numbers
  (signature
   (mixed empty-list
          cons-list)))

; Die leere Liste hat keine Eigenschaften
(define-record empty-list
  make-empty
  empty?)


