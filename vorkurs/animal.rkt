#lang deinprogramm/sdp/beginner
; Tier auf dem texanischen Highway ist eins der folgenden:
; - Gürteltier - ODER -
; - Papagei
; Fallunterscheidung
; gemischte Daten
(define animal
  (signature (mixed dillo parrot)))

#|
interface Animal { Animal runOver(); Animal feed(Weight weight); }
class Dillo implements Animal { ... }
class Parrot implements Animal { ... }
class Rattlesnake implements Animal { ... }
|#

(define weight (signature number))

; Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot
; - Gewicht
; ^^^ Zustand des Gürteltiers zu einem bestimmten Zeitpunkt
; zusammengesetzte Daten
(define-record dillo
  make-dillo
  dillo? ; Prädikat
  (dillo-alive? boolean)
  (dillo-weight number))

(: make-dillo (boolean number -> dillo))
(: dillo-alive? (dillo -> boolean))
(: dillo-weight (dillo -> number))

; lebendiges Gürteltier, 10kg
(define dillo1 (make-dillo #t 10))
; totes Gürteltier, 8kg
(define dillo2 (make-dillo #f 8))

; Gürteltier überfahren
; Java: void runOver() { this.alive = false; }
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))
(check-expect (run-over-dillo dillo2) dillo2)

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))

(: feed-dillo (dillo weight -> dillo))

(check-expect (feed-dillo dillo1 2) (make-dillo #t 12))
(check-expect (feed-dillo dillo2 5) dillo2)

#;(define feed-dillo
  (lambda (dillo food-weight)
    (make-dillo (dillo-alive? dillo)
                (if (dillo-alive? dillo)
                    (+ (dillo-weight dillo)
                       food-weight)
                    (dillo-weight dillo)))))

(define feed-dillo
  (lambda (dillo food-weight)
    (match dillo
      ((make-dillo #t weight) (make-dillo #t (+ weight food-weight)))
      ((make-dillo #f weight) dillo))))

; Papagei hat folgende Eigenschaften:
; - Satz
; - Gewicht
(define-record parrot
  make-parrot
  parrot?
  (parrot-sentence string)
  (parrot-weight number))

(: parrot? (any -> boolean))

; Begrüßungspapagei, 1kg
(define parrot1 (make-parrot "Hello!" 1))
; dicker Papagei, gemein
(define parrot2 (make-parrot "Mike ist doof!" 2))

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot2)
              (make-parrot "" 2))

(define run-over-parrot
  (lambda (parrot)
    (make-parrot "" (parrot-weight parrot))))


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





; Eine Liste ist eins der folgenden:
; - die leere Liste
; - eine Cons-Liste bestehend aus erstem Element und Rest-Liste