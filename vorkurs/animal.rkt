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
;                                                         ^^^^^ Selbstbezug

; Anfang: Liste aus Zahlen
(define list-of
  (lambda (element)
    (signature (mixed empty-list
                      (cons-list-of element)))))

; die leere Liste
(define-record empty-list
  make-empty
  empty?)

(define empty (make-empty))

; Eine Cons-Liste besteht aus:
; - erstes Element
; - Rest-Liste
(define-record (cons-list-of element) ; macht intern ein lambda
  cons
  cons?
  (first element)
  (rest (list-of element))) ; Polymorphie

; 1elementige Liste: 5
(define list1 (cons 5 empty))
; 2elementige Liste: 8 5
(define list2 (cons 8 (cons 5 empty)))
; 3elementige Liste: 7 8 5
;(define list3 (cons 7 (cons 8 (cons 5 empty))))
(define list3 (cons 7 list2))
(define list3* (cons 4 list2))
(define list4 (cons 2 list3))

; alle Elemente einer Liste addieren
(: list-sum ((list-of number) -> number))

(check-expect (list-sum list4) 22)

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0) ; "neutrales Element"
      ((cons? list)
       (+ (first list)
          (list-sum (rest list)))))))

(define list-of-numbers (signature (list-of number)))

; alle Elemente einer Liste multiplizieren
(: list-product (list-of-numbers -> number))

(check-expect (list-product list4) 560)

; neutrales Element bezüglich +:
; n + x = x + n = x für alle x --> 0
; neutrales Element bezüglich *:
; n * x = x * n = x für alle x --> 1

(define list-product
  (lambda (list)
    (cond
      ((empty? list) 1)
      ((cons? list)
       (* (first list)
          (list-product (rest list)))))))

; alle ungeraden Zahlen aus einer Liste extrahieren
(: list-odds (list-of-numbers -> list-of-numbers))

(check-expect (list-odds list4) (cons 7 (cons 5 empty)))

(define list-odds
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (odd? (first list))
           (cons (first list) (list-odds (rest list)))
           (list-odds (rest list)))))))

; Higher-Order-Funktion / Funktion höherer Ordnung
(: list-extract ((%element -> boolean) (list-of %element) -> (list-of %element)))

(define list-extract
  (lambda (p? list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (p? (first list))
           (cons (first list) (list-extract p? (rest list)))
           (list-extract p? (rest list)))))))

(: highway (list-of animal))
(define highway (cons dillo1 (cons dillo2 (cons parrot1 (cons parrot2 empty)))))
(: dillos (list-of dillo))
(define dillos (cons dillo1 (cons dillo2 empty)))