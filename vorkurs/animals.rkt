#lang deinprogramm/sdp/beginner

; Tiere auf dem texanischen Highway, eins der folgenden:
; - Gürteltier - ODER -
; - Papagei
; Fallunterscheidung
; hier: gemischte Daten
(define animal
  (signature (mixed dillo parrot)))

; Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot - UND -
; - Gewicht
; zusammengesetzte Daten
(define-record dillo
  make-dillo
  dillo? ; Prädikat
  (dillo-alive? boolean)
  (dillo-weight number))

(: make-dillo (boolean number -> dillo))
(: dillo-alive? (dillo -> boolean))
(: dillo-weight (dillo -> number))

(: dillo? (any -> boolean))

; lebendiges Gürteltier, 10kg
(define dillo1 (make-dillo #t 10))
; totes Gürteltier, 8kg
(define dillo2 (make-dillo #f 8))

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))
(check-expect (run-over-dillo dillo2)
              dillo2)

; Schablone
#;(define run-over-dillo
  (lambda (dillo)
    (make-dillo ... ...)
    ...
    (dillo-alive? dillo)
    (dillo-weight dillo)))

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))



; Absolutbetrag
(: absl (number -> number))

(check-expect (absl 5) 5)
(check-expect (absl -5) 5)

(define absl
  (lambda (n)
    ; binäre Verzweigung
    #;(cond
      ((>= n 0) n)
      (else (- n)))
    (if (>= n 0)
        n
        (- n))
    ))

; Gürteltier füttern
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 5)
              (make-dillo #t 15))
(check-expect (feed-dillo dillo2 5)
              dillo2)

(define feed-dillo
  (lambda (dillo amount)
    (define alive? (dillo-alive? dillo))
    (define weight (dillo-weight dillo))
    (make-dillo alive?
                (if alive?
                    (+ weight amount)
                    weight))))


; Ein Papagei hat folgende Eigenschaften:
; - Satz
; - Gewicht
(define-record parrot
  make-parrot parrot?
  (parrot-sentence string)
  (parrot-weight number))

; Begrüßungspapagei, 1kg
(define parrot1 (make-parrot "Hello!" 1))
; dicker Papagei
(define parrot2 (make-parrot "Goodbye!" 3))

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot1)
              (make-parrot "" 1))

(define run-over-parrot
  (lambda (parrot)
    (make-parrot  "" (parrot-weight parrot))))

; Tier überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal dillo1)
              (run-over-dillo dillo1))
(check-expect (run-over-animal parrot1)
              (run-over-parrot parrot1))

; Schablone
#;(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) ...)
      ((parrot? animal) ...))))

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (run-over-dillo animal))
      ((parrot? animal) (run-over-parrot animal)))))

; Tier füttern

; Klassiker:
; gemischte Daten, jeder Fall zusammengesetzte Daten

#|
interface Animal {
   Animal runOver();
   Animal feed(double amount);
}
class Dillo implements Animal {
   Animal runOver() { ... }
}
class Parrot implements Animal {
   Animal runOver() { ... }
}

// Open/Closed Principle
class Snake implements Animal { ... }

|#

; Eine Liste ist der folgenden:
; - die leere Liste
; - eine Cons-Liste bestehend aus erstem Element und Rest-Liste
;                                                         ^^^^^
(define list-of-numbers
  (signature (mixed empty-list cons-list)))

; Die leere Liste ...
(define-record empty-list
  make-empty empty?)

(: empty? (any -> boolean))

(define empty (make-empty))

; Eine Cons-Liste besteht aus:
; - erstes Element
; - Rest-Liste
(define-record cons-list
  cons cons?
  (first number)
  (rest list-of-numbers))

; 1elementige Liste: 5
(define list1 (cons 5 empty))
; 2elementige Liste: 5 8
(define list2 (cons 5 (cons 8 empty)))
; 3elementige Liste: 5 8 3
(define list3 (cons 5 (cons 8 (cons 3 empty))))
; 4elementige Liste: 2 5 8 3
(define list4 (cons 2 list3))

; Elemente einer Liste addieren
(: list-sum (list-of-numbers -> number))

(check-expect (list-sum list4) 18)

#;(define list-sum
  (lambda (list)
    (cond
      ((empty? list) ...)
      ((cons? list)
       ...
       (first list)
       (list-sum (rest list))
       ...))))


(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0)
      ((cons? list)
       (+
        (first list)
        (list-sum (rest list)))))))

; Elemente einer Liste multiplizieren
(: list-product (list-of-numbers -> number))

(check-expect (list-product list4) 240)

(define list-product
  (lambda (list)
    (cond
      ((empty? list) 1)
      ((cons? list)
       (*
        (first list)
        (list-product (rest list)))))))


; n neutrales Element:
; n + x = x + n = x für alle Zahlen n
; => n = 0

; n * x = x * n = x
; => n = 1

; Aus einer Liste von Zahlen die ungeraden Zahlen extrahieren
(: extract-odds (list-of-numbers -> list-of-numbers))

(check-expect (extract-odds list4)
              (cons 5 (cons 3 empty)))

(define extract-odds
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (odd? (first list))
           (cons (first list)
                 (extract-odds (rest list)))
           (extract-odds (rest list)))))))

; Alle Elemente einer Liste extrahieren, die ein "Kriterium"
(: extract ((number -> boolean) list-of-numbers  -> list-of-numbers))
; Higher-Order-Funktion

(check-expect (extract even? list4) (cons 2 (cons 8 empty)))
(check-expect (extract odd? list4) (cons 5 (cons 3 empty)))

(define extract
  (lambda (p? list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (p? (first list))
           (cons (first list)
                 (extract p? (rest list)))
           (extract p? (rest list)))))))

(define highway (cons dillo1 (cons dillo2 (cons parrot1 (cons parrot2 empty)))))