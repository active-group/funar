#lang deinprogramm/sdp/beginner

; Haustier _ist eins der Folgenden_:
; - Katze -ODER-
; - Hund -ODER-
; - Schlange
(define pet
  (signature (enum "cat" "dog" "snake")))

; Ist ein Haustier niedlich?
(: cute? ; <- cute-p
   (pet -> boolean))

(check-expect (cute? "dog")
              #t)
(check-expect (cute? "cat")
              #t)
(check-expect (cute? "snake")
              #f)

; Gerüst zuerst
#;(define cute?
    (lambda (pet)
      ...))

; bei Daten, die Fallunterscheidung sind ->
; Fälle unterschiedlich behandeln
; -> cond
; Schablone:
(define cute?
  (lambda (pet)
    (cond ; Verzweigung: ein Paar pro Fall
      ((string=? pet "dog") #t) ; (<Beding> <Erg>)
      ((string=? pet "snake") #f)
      ((string=? pet "cat") #t))))

; Digitaluhr (Uhrzeit) hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten -> Record
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour natural) ; Selektoren
  (time-minute natural))

; optional: Signaturen für Record-Teile
(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

(define time1 (make-time 12 23))
(define time2 (make-time 15 11))

; Minuten seit Mitternacht ausrechnen
(: minutes-since-midnight (time -> natural))

(check-expect (minutes-since-midnight time1)
              743)
(check-expect (minutes-since-midnight time2)
              911)

; Brauchen: Schablone f. zusammengesetzte Daten
; - müssen Bestandteile anschauen
#;(define minutes-since-midnight
    (lambda (time)
      ... (time-hour time) ... (time-minute time) ...))

(define minutes-since-midnight
  (lambda (time)
    (+ (* 60
          (time-hour time))
       (time-minute time))))

; Übung:
; Minuten seit Mitternacht rein -> time raus
; Namensvorschlag: minutes-since-midnight->time
(: msm->time (natural -> time))

; hier wird strukturell auf Gleicheit geprüft
(check-expect (msm->time 743)
              time1)
(check-expect (msm->time 911)
              time2)

(define msm->time
  (lambda (minutes)
    (make-time (quotient minutes 60)
               (remainder minutes 60))))

; gemischte Daten

; Tiere auf dem texanischen Highway
; Ein Gürteltier hat folgende Eigenschaften
; - lebendig oder tot?
; - Gewicht
; -> wieder zusammengesetzte Daten

(define-record dillo
  make-dillo
  dillo? ; <- nachträglich eingefügt: Prädikat
  (dillo-alive? boolean)
  (dillo-weight natural))

(define dillo1 (make-dillo #t 10))
(define dillo2 (make-dillo #f 5))

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))
(check-expect (run-over-dillo dillo2)
              dillo2)

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f
                (dillo-weight dillo))))

; Gürteltier füttern

#;(if alive? ; Prädikat
      3 ; then-Fall
      5) ; else-Fall
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 3)
              (make-dillo #t 13))
(check-expect (feed-dillo dillo2 5)
              dillo2)

(define feed-dillo
  (lambda (dillo amount)
    #;(cond
        ((dillo-alive? dillo)
         (make-dillo #t (+ amount
                           (dillo-weight dillo))))
        (#t dillo))
    (if (dillo-alive? dillo)
        (make-dillo #t (+ amount
                          (dillo-weight dillo)))
        dillo)))

; Ein Papagei hat folgende Eigenschaften
; - ein Satz, den er sagen kann
; - ein Gewicht
(define-record parrot
  make-parrot
  parrot?
  (parrot-sentence string)
  (parrot-weight number))

; Ein Begrüßungspapagei
(define parrot1 (make-parrot "Hallo!" 2))
; Verabschiedungspapagei
(define parrot2 (make-parrot "Ciao" 1))

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot1)
              (make-parrot "" 2))
(check-expect (run-over-parrot parrot2)
              (make-parrot "" 1))

(define run-over-parrot
  (lambda (parrot)
    (make-parrot "" (parrot-weight parrot))))

; Ein Tier ist eins der Folgenden
; - Ein Gürteltier -ODER-
; - Ein Papagei
; -> gemischte Daten -> mixed
(define animal
  (signature (mixed dillo parrot)))

; Tiere überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal dillo1)
              (run-over-dillo dillo1))
(check-expect (run-over-animal dillo2)
              (run-over-dillo dillo2))
(check-expect (run-over-animal parrot1)
              (run-over-parrot parrot1))
(check-expect (run-over-animal parrot2)
              (run-over-parrot parrot2))

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (run-over-dillo animal))
      ((parrot? animal) (run-over-parrot animal)))))

;(define feed-animal ...)

#|
interface Animal { Animal runOver(); Animal petAnimal(); }

class Dillo implements Animal { @Override Animal runOver(); petAnimal  }
class Parrot implements Animal { ... }
class Snake implements Animal { ... }

Tradeoffs:
- OOP: neue Fälle einfach, neue Operationen "schwer"
- FP: neue Operationen einfach, neue Fälle "schwer"

Präziser:

Neue Fälle (neues Tier):
- FP wie oben: (Anzahl Fallunterscheidungen) Codestellen müssen "angefasst" werden,
  nämlich jede Funktion, die (: foo (animal -> x)) erfüllt
- OOP wie oben: _eine_ Codestelle muss angefasst werden, nämlich
  die neue Klasse

Neue Funktionalität (neue Funktion (animal -> x)):
- FP wie oben: _eine_ Codestelle muss angefasst werden, nämlich die Funktion
- OOP wie oben: (Anzahl Tiere) Codestellen müssten angefasst werden

ABER: in beiden Paradigmen gibt es natürlich Lösungen/Muster/Ansätze!

Siehe: Expression problem
|#


;; LISTEN

; Eine Liste ist eins der Folgenden:
; - die leere Liste
; - eine Cons-Liste, bestehend aus erstem Element und einer Rest-Liste
(define list-of
  (lambda (element)
    (signature (mixed empty-list
                      (cons-list-of element)))))

(define list-of-numbers
  (list-of number))

; Die leere Liste....
(define-singleton empty-list
  empty ; kein richtiger Konstruktor: "das Singleton"
  empty?)

; Eine Cons-Liste hat folgende Eigenschaften:
; - erstes Element
; - Rest-Liste
(define-record (cons-list-of element)
  cons ; histor. Gründe
  cons?
  (first element) ; car, cadr, caddr, cadddr, cadadadr
  (rest (list-of element))) ; cdr

; List mit einem Element
(define list1 (cons 5 empty))
(define list2 (cons 5 (cons 8 empty)))
(define list3 (cons 6 (cons 3 (cons 4 empty))))
(define list4 (cons 5 list3))

; Summe der Listenelement ermitteln
(: list-sum (list-of-numbers -> number))

(check-expect (list-sum empty) 0)
(check-expect (list-sum list1) 5)
(check-expect (list-sum list2) 13)
(check-expect (list-sum list3) 13)
(check-expect (list-sum list4) 18)

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0) ; 0 ist das neutrale Element der Addition
      ((cons? list)
       (+ (first list)
          (list-sum (rest list))))))) ; Schablone: rek. Selbstaufruf vor (rest list)

; Produkt der Listenelemente
(: list-product (list-of-numbers -> number))

(check-expect (list-product list4)
              360)
(check-expect (list-product empty)
              1)

(define list-product
  (lambda (list)
    (cond
      ((empty? list) 1) ; 1 ist das neutrale Element der Multiplikation
      ((cons? list)
       (* (first list)
          (list-product (rest list)))))))

; Alle ungeraden Zahlen aus einer Liste extrahieren
(: extract-odds (list-of-numbers -> list-of-numbers))

(check-expect (extract-odds list4)
              (cons 5 (cons 3 empty)))

(define extract-odds
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (even? (first list))
           (extract-odds (rest list))
           (cons (first list)
                 (extract-odds (rest list))))))))

(define extract-evens
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (odd? (first list))
           (extract-odds (rest list))
           (cons (first list)
                 (extract-odds (rest list))))))))

; Signaturvariable
; Higher-Order-Funktion
(: extract ((%a -> boolean) (list-of %a) -> (list-of %a)))

; Gehen so vor wie bei tile (Mike)

(check-expect (extract odd? list4)
              (extract-odds list4))
(check-expect (extract (lambda (n) (> n 5)) list4)
              (cons 6 empty))

(define extract
  (lambda (p? list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (p? (first list))
           (cons (first list)
                 (extract p? (rest list)))
           (extract p? (rest list)))))))

; Alle Zahlen einer Liste verdoppeln
(: double-all (list-of-numbers -> list-of-numbers))

(check-expect (double-all empty)
              empty)
(check-expect (double-all (cons 3 empty))
              (cons 6 empty))

(define double-all
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons (* 2 (first list))
             (double-all (rest list)))))))

; Alle Elemente einer Liste "verändern"
(: list-map ((%a -> %b) (list-of %a) -> (list-of %b)))

(check-expect (list-map (lambda (n) (* n 2)) list4)
              (double-all list4))
(check-expect (list-map (lambda (s) (string-append s "foo"))
                        (cons "abc" (cons "def" empty)))
              (cons "abcfoo" (cons "deffoo" empty)))

(define list-map
  (lambda (f list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons (f (first list))
             (list-map f (rest list)))))))

; Abstraktion über list-sum und list-product
(: foo (... ... (list-of %a) ->
(define list-product
  (lambda (... ... list)
    (cond
      ((empty? list) 1) ; 1 ist das neutrale Element der Multiplikation
      ((cons? list)
       (* (first list)
          (list-product (rest list)))))))