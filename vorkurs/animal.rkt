#lang deinprogramm/sdp/beginner

; Datenanalyse

; Datendefinition (Kommentar)

; Haustier ist eins der folgenden:
; - Katze - ODER -
; - Hund - ODER -
; - Schlange
; Fallunterscheidung
; hier speziell: Aufzählung
(define pet
  (signature (enum "cat" "dog" "snake")))

; Ist Haustier niedlich?
(: cute? (pet -> boolean))

(check-expect (cute? "cat")
              #t)
(check-expect (cute? "dog")
              #t)
(check-expect (cute? "snake")
              #f)

; Gerüst
#;(define cute?
  (lambda (pet)
    ...))

; Schablone
#;(define cute?
  (lambda (pet)
    ; Fallunterscheidung in den Daten
    ; => in der Funktion: Verzweigung
    ; 1 Zweig pro Fall
    (cond
      ((string=? pet "cat") ...) ; (<Bedingung> <Ergebnis>)
      ((string=? pet "dog") ...)
      ((string=? pet "snake") ...))))

(define cute?
  (lambda (pet)
    ; Fallunterscheidung in den Daten
    ; => in der Funktion: Verzweigung
    ; 1 Zweig pro Fall
    (cond
      ((string=? pet "cat") #t) ; (<Bedingung> <Ergebnis>)
      ((string=? pet "dog") #t)
      ((string=? pet "snake") #f))))

; Uhrzeit hat folgende Eigenschaften:
; - Stunde - UND -
; - Minute
; zusammengesetzte Daten
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour natural) ; Selektor
  (time-minute natural))

; natural: Signatur für natürliche Zahlen / Zählzahlen

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))


; 12 Uhr 24 Minuten
(define time1 (make-time 12 24))
; 15:11
(define time2 (make-time 15 11))

; Minuten seit Mitternacht
(: msm (time -> natural))

(check-expect (msm time1)
              744)
(check-expect (msm time2)
              (+ (* 15 60) 11))

; Schablone:
#;(define msm
  (lambda (time)
    ... (time-hour time) ... (time-minute time) ...))

(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

; Tiere auf dem texanischen Highway

; Tier ist eins der folgenden:
; - Gürteltier - ODER -
; - Papagei
; Fallunterscheidung
; gemischte Daten
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

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))

; Gürteltier füttern
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 2)
              (make-dillo #t 12))
(check-expect (feed-dillo dillo2 2)
              dillo2)

(define feed-dillo
  (lambda (dillo amount)
    (make-dillo (dillo-alive? dillo)
                (cond
                  ((dillo-alive? dillo) (+ (dillo-weight dillo)
                                           amount))
                  ((not (dillo-alive? dillo)) (dillo-weight dillo))))))

#;(define feed-dillo
  (lambda (dillo amount)
    ; lokale Variablen
    (define alive? (dillo-alive? dillo))
    (define weight (dillo-weight dillo))
    (make-dillo alive?
                ; binäre Verzweigung:
                ; (if <Bedingung> <Konsequente> <Alternative>)
                (if alive?
                    (+ weight amount)
                    weight))))

; Ein Papagei hat folgende Eigenschaften:
; - Satz - UND -
; - Gewicht
(define-record parrot
  make-parrot
  parrot?
  (parrot-sentence string)
  (parrot-weight number))

; Begrüßungs-Papagei, 1kg
(define parrot1 (make-parrot "Hallo!" 1))
; Verabschiedungs-Papagei, 2kg
(define parrot2 (make-parrot "Tschüss!" 2))

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot1)
              (make-parrot "" 1))

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

; Fallunterscheidungen ("ODER" / "eins der folgenden")
; vs.
; zusammengesetzte Daten ("UND" / "besteht aus", "hat folgende Eigenschaften")

#|
interface Animal {
  Animal runOver();
  Animal feed(double amount);
}
class Dillo implements Animal { Animal runOver() { ... } }
class Parrot implements Animal { Animal runOver() { ... } }

class Snake implements Animal { Animal runOver() { ... } }

FP vs. OOP:

- In FP ist es "billig", neue Funktionen zu definieren.
- In OOP ist es "billig", neue Klassen zu definieren.

Schön wäre: beides billig, "expression problem"



|#

; Eine Liste ist eins der folgenden:
; - die leere Liste
; - eine Cons-Liste aus erstem Element und Rest-Liste
;                                               ^^^^^ Selbstbezug

; zunächst nur Listen aus Zahlen
(define list-of-numbers
  (signature (mixed empty-list cons-list)))

; "die leere Liste": Singleton
(define-record empty-list
  make-empty empty?)

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
; 2elementige Liste: 2 5
(define list2 (cons 2 (cons 5 empty)))
; 3elementige Liste: 7 2 5
(define list3 (cons 7 (cons 2 (cons 5 empty))))
; 4elementige Liste: 6 7 2 5
(define list4 (cons 6 list3))

; Elemente einer Liste aufsummieren
(: list-sum (list-of-numbers -> number))

(check-expect (list-sum list4) 20)

; Schablone:
#;(define list-sum
  (lambda (list)
    (cond
      ((empty? list) ...)
      ((cons? list) ...
       ...
       (first list)
       (list-sum (rest list))
       ...))))

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0)
      ((cons? list)
       (+ (first list)
          (list-sum (rest list)))))))






