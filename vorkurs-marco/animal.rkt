#lang deinprogramm/sdp/beginner

; hunde, katzen, schlangen

; Datendefinition
; Ein Haustier ist eines der folgenden
; - Hund - ODER -
; - Katze - ODER -
; - Schlange - ODER -
; - Fruchtfliege
; Fallunterscheidung
; speziell hier: Aufzählung
(define pet
  (signature (enum "dog" "cat" "snake" "fruit fly")))

; Konstruktionsanleitung
; 1. Kurzbeschreibung schreiben
; 2. Signatur
; 3. Test
; 4. Gerüst

; Ist ein Haustier niedlich?
(: cute? (pet -> boolean))
#;(: cute? ((enum "dog" "cat" "snake" "fruit fly") -> boolean))

(check-expect (cute? "dog") #t)
(check-expect (cute? "cat") #t)
(check-expect (cute? "snake") #t)
(check-expect (cute? "fruit fly") #f)

(define cute?
  (lambda (animal)
    (cond
      ((string=? "dog" animal) #t)
      ((string=? "cat" animal) #t)
      ((string=? "snake" animal) #t)
      ((string=? "fruit fly" animal) #f))))

; Schablone für gemischte Daten/Falluntescheidung

#;(define XXX
    (lambda (l)
      (cond
        (fall1 konsequente1)
        (fall2 konsequente2))))


; Uhrzeiten
(define hour (signature (integer-from-to 0 23)))
(define minute (signature (integer-from-to 0 59)))

; Eine Uhrzeite besteht aus
; - Stunde - UND -
; - Minuten
; zusammengesetzte Daten
(define-record time  ; <- signatur
  make-time ; <- Konstruktor
  (time-hour hour) ; <- Selektoren
  (time-minute minute))

(: make-time (hour minute -> time))
(: time-hour (time -> hour))
(: time-minute (time -> minute))

; 10:49
(define time1 (make-time 10 49))
; fünf vor 12
(define time2 (make-time 23 55))


; Minuten seit Mitternacht
(: since-midnight (time -> natural))

(check-expect (since-midnight time1)
              649)
(check-expect (since-midnight time2)
              1435)
(check-expect (since-midnight (make-time 0 0))
              0)

(define since-midnight
  (lambda (time)
    (+ (* (time-hour time)
          60)
       (time-minute time))))

; Aufgabe: Aus Minuten nach Mitternacht die Uhrzeit berechnen.
(: msm->time (natural -> time))

(check-expect (msm->time 649)
              time1)
(check-expect (msm->time 1435)
              time2)
(check-expect (msm->time 0)
              (make-time 0 0))

(define msm->time
  (lambda (minutes)
    (make-time (quotient minutes 60)
               (remainder minutes 60))))






; Tiere auf dem texanischen Highway sind eines der folgenden
; - ein Gürteltier - ODER -
; - ein Papagei
; gemischte Daten

(define animal
  (signature (mixed dillo parrot)))

; Ein Gürteltier besteht aus
; - lebendig oder tot? - UND -
; - Gewicht
; zusammengesetzen Daten

(define-record dillo ; signatur
  make-dillo ; konstruktor
  dillo? ; prädikat
  (dillo-alive? boolean)
  (dillo-weight number))

(: make-dillo (boolean number -> dillo))
; dillos sind immer #t, alles andere ist #f
(: dillo? (any -> boolean))
; foo instanceOf Dillo

; lebendiges dillo mit 33kg
(define dillo1 (make-dillo #t 33))
(define dillo2 (make-dillo #f 15))

; Ein Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 33))
(check-expect (run-over-dillo dillo2)
              dillo2)

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f
                (dillo-weight dillo))))

; Ein Gürteltier füttern
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 5)
              (make-dillo (dillo-alive? dillo1)
                          (+ 5 (dillo-weight dillo1))))
(check-expect (feed-dillo dillo2 5)
              dillo2
              #;(make-dillo (dillo-alive? dillo2)
                          (dillo-weight dillo2)))

(define feed-dillo
  (lambda (dillo amount)
    (define alive? (dillo-alive? dillo))
    (define weight (dillo-weight dillo))
    (make-dillo alive?
                (if alive?
                    (+ weight amount)
                    weight))))
    #;(cond
      ((dillo-alive? dillo)
       (make-dillo #t
                   (+ amount (dillo-weight dillo))))
      (else dillo))

; Papagei besteht aus
; - ein Satz den er sprechen kann - UND -
; - Gewicht
; zusammengesetzte Daten
(define-record parrot
  make-parrot
  parrot? ; prädikat
  (parrot-sentence string)
  (parrot-weight number))

(define hallo (make-parrot "Hi!" 3))
(define ciao (make-parrot "Ciao!" 2))

; Einen Papagei überfahren
(: run-over-parrot (parrot -> parrot))
(check-expect (run-over-parrot hallo)
              (make-parrot ""
                           3))
(check-expect (run-over-parrot ciao)
              (make-parrot ""
                           2))

(define run-over-parrot
  (lambda (parrot)
    (make-parrot "" (parrot-weight parrot))))

; Ein Tier auf dem texanischen Highway überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal dillo1)
              (make-dillo #f 33))
(check-expect (run-over-animal hallo)
              (make-parrot ""
                           3))

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (run-over-dillo animal))
      ((parrot? animal) (run-over-parrot animal)))))

#;(define feed-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (feed-dillo animal))
      ((parrot? animal) (feed-parrot animal)))))
#|

interface Animal { Animal runOver(); Animal feed(); }

class Dillo implements Animal { @Override Animal runOver(); }
class Parrot implements Animal { ... }
class Snake implements Animal { ... }

Expression Problem:

- OOP: neue Fälle hinzufügen super easy, neue Funktion aufwändig
- FP: neue Fälle schwierig, neue Funktion weniger aufwändig
|#



; listen

; Liste ist eines der folgenden:
; - die leere Liste
; - Cons-Liste, besteht aus erstem element und rest-liste
;                                                   ^^^^^
; daten mit selbstbezug
; gemischte daten
#;(define list-of-numbers
  (signature (mixed empty-list cons-list)))

(define list-of
  (lambda (element)
    (signature (mixed empty-list
                      (cons-list-of element)))))

(define list-of-numbers (list-of number))

; Die leere Liste
(define-singleton empty-list ; signatur
  empty ; "das singleton"
  empty?) ; prädikat

; Eine Cons-Liste besteht aus:
; - erstem Element
; - Rest-Liste
#;(define-record cons-list
  cons
  cons?
  (first number)
  (rest list-of-numbers))
(define-record (cons-list-of element) ; macht für uns ein lambda
  cons
  cons?
  (first element)
  (rest (list-of element)))

; [5]
(define list1 (cons 5 empty))
; [5 8]
(define list2 (cons 5 (cons 8 empty)))
(define list3 (cons 1 list2))

; Summe der Listenelemente
(: list-sum (list-of-numbers -> number))

(check-expect (list-sum empty)
              0) ; neutrales element im monoid (natural, +)
(check-expect (list-sum list1)
              5)
(check-expect (list-sum list3)
              14)

#;(define list-sum
  ; gemischte daten
  ; -> zusammengesetzte daten mit selbstbezug
  (lambda (list)
    (cond
      ((empty? list) 0)
      ((cons? list)
       (+ (first list)
          (list-sum (rest list)))))))
(define list-sum
  (lambda (list)
    (list-fold + 0 list)))

; Aufgabe 1: Produkt der Listenelemente.
(: list-product (list-of-numbers -> number))

(check-expect (list-product empty)
              1) ; neutrales element im monoid (natural, *)
(check-expect (list-product list1)
              5)
(check-expect (list-product list3)
              40)

#;(define list-product
  (lambda (list)
    (cond
      ((empty? list) 1)
      ((cons? list)
       (* (first list)
          (list-product (rest list)))))))
(define list-product
  (lambda (list)
    (list-fold * 1 list)))

; Abstraktion:
; 1. letztes Mal kopieren
; 2. Unterschieden Namen geben
; 3. lambda

; Eine Liste zusammenfalten
(: list-fold
   ((%a %b -> %b) %b (list-of %a) -> %b))

(check-expect (list-fold + 0 empty)
              0)
(check-expect (list-fold + 0 list3)
              14)
(check-expect (list-fold * 1 empty)
              1)
(check-expect (list-fold * 1 list3)
              40)
(check-expect (list-fold string-append
                         ""
                         (cons "Marco" (cons "Schneider" empty)))
              "MarcoSchneider")

(define list-fold ; auch bekannt als reduce, ...
  (lambda (op zero list)
    (cond
      ((empty? list) zero)
      ((cons? list)
       (op (first list)
           (list-fold op zero (rest list)))))))

#;(define list-up-to
  (lambda (n)
    (if (= n 0)
        empty
        (cons n (list-up-to (- n 1))))))

; Aufgabe 2: Aus einer Liste alle geraden Zahlen extrahieren.
(: extract-evens (list-of-numbers -> list-of-numbers))

(check-expect (extract-evens (cons 1 (cons 2 (cons 3 empty))))
              (cons 2 empty))
(check-expect (extract-evens empty) empty)

#;(define extract-evens
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (even? (first list))
           (cons (first list)
                 (extract-evens (rest list)))
           (extract-evens (rest list)))))))
(define extract-evens
  (lambda (list)
    (list-extract even? list)))

; Aus einer Liste alle ungeraden Zahlen extrahieren.
(: extract-odds (list-of-numbers -> list-of-numbers))

(check-expect (extract-odds (cons 1 (cons 2 (cons 3 empty))))
              (cons 1 (cons 3 empty)))
(check-expect (extract-odds empty) empty)

#;(define extract-odds
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (odd? (first list))
           (cons (first list)
                 (extract-evens (rest list)))
           (extract-evens (rest list)))))))
(define extract-odds
  (lambda (list)
    (list-extract odd? list)))

; eine Liste mit einem Prädikat filtern
#;(: list-extract ((number -> boolean)
                 list-of-numbers
                 -> list-of-numbers))
(: list-extract ((%a -> boolean) (list-of %a) -> (list-of %a)))
                 
(check-expect (list-extract odd?
                            (cons 1 (cons 2 (cons 3 empty))))
              (cons 1 (cons 3 empty)))
(check-expect (list-extract even?
                            (cons 1 (cons 2 (cons 3 empty))))
              (cons 2 empty))

(define list-extract ; auch bekannt als filter, ...
  (lambda (predicate list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (predicate (first list))
           (cons (first list)
                 (list-extract predicate (rest list)))
           (list-extract predicate (rest list)))))))

; wende eine funktion auf alle werte einer liste an
(: list-map ((%a -> %b) (list-of %a) -> (list-of %b)))

(define inc (lambda (n) (+ n 1)))

(check-expect (list-map inc (cons 1 (cons 2 empty)))
              (cons 2 (cons 3 empty)))
(check-expect (list-map inc empty)
              empty)
(check-expect (list-map odd? (cons 1 (cons 2 (cons 3 empty))))
              (cons #t (cons #f (cons #t empty))))

(define list-map
  (lambda (f list)
    (cond
      ((empty? list) empty)  ; könnte auch list sein
      ((cons? list)
       (cons
        (f (first list))
        (list-map f (rest list)))))))

; Eine Liste von Tieren auf dem texanischen Highway überfahren
(define highway (cons dillo1 (cons dillo2 (cons hallo (cons ciao empty)))))

(: run-over-animals ((list-of animal) -> (list-of animal)))

(check-expect (run-over-animals highway)
              (cons (make-dillo #f 33)
                    (cons dillo2
                          (cons (make-parrot "" 3)
                                (cons (make-parrot "" 2)
                                      empty)))))

(define run-over-animals
  (lambda (animals)
    (list-map run-over-animal animals)))


; die summe aller geraden zahlen
(: sum-evens (list-of-numbers -> number))

(check-expect (sum-evens empty) 0)
(check-expect (sum-evens (cons 1 (cons 2 (cons 3 (cons 4 empty)))))
              6)
(define sum-evens
  (lambda (list)
    (list-fold + 0 (list-extract even? list))))





