#lang deinprogramm/sdp/beginner

; Datenanalyse

; Datendefinition:
; Ein Haustier ist eins der folgenden:
; - Hund - ODER -
; - Katze - ODER -
; - Schlange
; Fallunterscheidung
; hier speziell: Aufzählung
(define pet
  (signature (enum "dog" "cat" "snake")))

; Ist ein Haustier niedlich?
(: cute? (pet -> boolean))

(check-expect (cute? "dog") #t)
(check-expect (cute? "cat") #t)
(check-expect (cute? "snake") #f)

; Gerüst:
#;(define cute?
  (lambda (pet)
    ...))

; Schablone
#;(define cute?
  (lambda (pet)
    (cond ; Verzweigung, 1 Zweig pro Fall
      ((string=? pet "dog") ...) ; (<Bedingung> <Ergebnis>)
      ((string=? pet "cat") ...)
      ((string=? pet "snake") ...))))

(define cute?
  (lambda (pet)
    (cond ; Verzweigung, 1 Zweig pro Fall
      ((string=? pet "dog") #t) ; (<Bedingung> <Ergebnis>)
      ((string=? pet "cat") #t)
      ((string=? pet "snake") #f))))

; Eine Stunde ist eine natürliche Zahl zwischen 0 und 23.
(define hour (signature (integer-from-to 0 23)))
(define minute (signature (integer-from-to 0 59)))
  
; Uhrzeit besteht aus/hat folgende Eigenschaften:
; - Stunde - UND -
; - Minute
; zusammengesetzte Daten
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour hour) ; Selektor
  (time-minute minute))

(: make-time (hour minute -> time))
(: time-hour (time -> hour))
(: time-minute (time -> minute))

; 11 Uhr 23
(define time1 (make-time 11 23))
; 14:11
(define time2 (make-time 14 11))

; Signaturverletzung
;(make-time 27 12)


; Minuten seit Mitternacht berechnen
(: msm (time -> natural))

(check-expect (msm time1)
              683)
(check-expect (msm time2)
              851)

; Schablone:
#;(define msm
  (lambda (time)
    ... (time-hour time) ...
    ... (time-minute time) ...))

(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

; Aus Minuten seit Mitternacht die Uhrzeit machen
(: msm->time (natural -> time))

(check-expect (msm->time 683)
              time1)
(check-expect (msm->time 851)
              time2)

; Schablone: Konstruktoraufruf
(define msm->time
  (lambda (minutes)
    (make-time (quotient minutes 60)
               (remainder minutes 60))))

; Tiere auf dem texanischen Highway

; Ein Tier ist eins der folgenden:
; - Gürteltiere - ODER -
; - Papagei
; Fallunterscheidung
; gemischte Daten (jeder Fall eigene Datendefinition)
(define animal
  (signature (mixed dillo parrot)))

; Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot? - UND -
; - Gewicht
; zusammengesetzte Daten

; eigentlich: (Wahrnehmung eines) Zustands des Gürteltiers zu einem bestimmten Zeitpunkt
(define-record dillo
  make-dillo
  dillo? ; Prädikat
  (dillo-alive? boolean)
  (dillo-weight number))

(: make-dillo (boolean number -> dillo))
(: dillo-alive? (dillo -> boolean))
(: dillo-weight (dillo -> number))

(: dillo? (any -> boolean))

(define make-default-dillo
  (lambda (weight)
    (make-dillo #t weight)))

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
    ... (dillo-alive? dillo) ...
    ... (dillo-weight dillo) ...))

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))

; Gürteltier füttern
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 5)
              (make-dillo #t 15))
(check-expect (feed-dillo dillo2 5)
              dillo2)

(define feed-dillo
  (lambda (dillo amount)
    #;(cond
      ((dillo-alive? dillo) (make-dillo #t (+ (dillo-weight dillo) amount)))
      (else dillo))
    (define alive? (dillo-alive? dillo))
    (define weight (dillo-weight dillo))
    #;(if alive?
        (make-dillo alive? (+ (dillo-weight dillo) amount))
        dillo)
    #;(if alive?
        (make-dillo alive? (+ weight amount))
        (make-dillo alive? weight))
    (make-dillo alive?
                (if alive?
                    (+ weight amount)
                    weight))))


; Ein Papagei hat folgende Eigenschaften:
; - ein Satz - UND -
; - ein Gewicht
; zusammengesetzte Daten
(define-record parrot
  make-parrot
  parrot?
  (parrot-sentence string)
  (parrot-weight number))

; Begrüßungspapagei, 1kg
(: parrot1 parrot)
(define parrot1 (make-parrot "Hello!" 1))
; Verabschiedungspapagei, 2kg
(: parrot2 animal)
(define parrot2 (make-parrot "Goodbye!" 2))

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot1)
              (make-parrot "" 1))
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

#|
interface Animal { Animal runOver(); }
class Dillo implements Animal { @Override Animal runOver(); }
class Parrot implements Animal { ... }

class Snake implements Animal { ... } 
|#

#|
Tradeoffs:

OOP: neue Fälle einfach, neue Operationen schwer
FP: neue Fälle schwer, neue Operationen einfach

"expression problem": beides soll einfach sein
|#


; Liste ist eins der folgenden:
; - die leere Liste
; - eine Cons-Liste, bestehend aus erstem Element und Rest-Liste
;                                                          ^^^^^
#;(define list-of-numbers
  (signature (mixed empty-list cons-list)))
(define list-of
  (lambda (element)
    (signature (mixed empty-list
                      (cons-list-of element)))))

; Die leere Liste ...
(define-singleton empty-list ; Signatur
  empty ; "das Singleton"
  empty?) ; Prädikat

; Eine Cons-Liste besteht aus:
; - erstes Element
; - Rest-Liste
(define-record (cons-list-of element) ; macht lambda für uns
  cons
  cons?
  (first element)
  (rest (list-of element))) ; Selbstbezug

(define list-of-numbers (signature (list-of number)))

; 1elementige Liste: 5
(define list1 (cons 5 empty))
; 2elementige Liste: 5 8
(define list2 (cons 5 (cons 8 empty)))
; 3elementige Liste: 6 3 4
(define list3 (cons 6 (cons 3 (cons 4 empty))))
; 4elementige Liste: 5 6 3 4
(define list4 (cons 5 list3))
; 0elementige Liste
(define list5 empty)

; Summe der Listenelemente
(: list-sum (list-of-numbers -> number))

(check-expect (list-sum list4)
              18)

(define list-sum
  (lambda (list)
    (cond
      ((empty? list)
       0) ; das neutrale Element in bezug auf + (Gruppentheorie - Algebra)
      ((cons? list)
       (+ (first list)
          (list-sum (rest list)))))))

; n + x = x + n = x

; Produkt der Listenelemente
(: list-product (list-of-numbers -> number))

(check-expect (list-product list4)
              360)

; Schablone:
#;(define list-product
  (lambda (list)
    (cond
      ((empty? list) ...)
      ((cons? list)
       ...
       (first list)
       (list-product (rest list))
       ...))))

(define list-product
  (lambda (list)
    (cond
      ((empty? list)
       1)
      ((cons? list)
       (* (first list)
          (list-product (rest list)))))))

; n * x = x * n = x

; Alle ungeraden Elemente aus einer Liste extrahieren
(: extract-odds (list-of-numbers -> list-of-numbers))

(check-expect (extract-odds list4)
              (cons 5 (cons 3 empty)))

(define extract-odds
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       ; (define rest-odds (extract-odds (rest list))) ; auch OK
       (if (odd? (first list))
           (cons (first list) (extract-odds (rest list)))
           (extract-odds (rest list)))))))

(: extract ((number -> boolean) list-of-numbers -> list-of-numbers))
   
(check-expect (extract even? list4)
              (cons 6 (cons 4 empty)))
(check-expect (extract odd? list4)
              (cons 5 (cons 3 empty)))

(define extract
  (lambda (p? list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       ; (define rest-odds (extract-odds (rest list))) ; auch OK
       (if (p? (first list))
           (cons (first list) (extract p? (rest list)))
           (extract p? (rest list)))))))

(define highway
  (cons dillo1 (cons dillo2 (cons parrot1 (cons parrot2 empty)))))