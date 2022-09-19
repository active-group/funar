#lang deinprogramm/sdp/beginner
; Datenanalyse

; Ein Haustier ist eins folgenden:
; - Hund - ODER -
; - Katze - ODER -
; - Schlange
; Fallunterscheidung
; speziell hier: Aufzählung
(define pet
  (signature (enum "dog" "cat" "snake")))

; Ist ein Haustier niedlich?
(: cute? (pet -> boolean))

(check-expect (cute? "dog") #t)
(check-expect (cute? "cat") #t)
(check-expect (cute? "snake") #f)

; Gerüst
#;(define cute?
  (lambda (pet)
    ...))

; Schablone
#;(define cute?
  (lambda (pet)
    (cond ; Verzweigung
      ((string=? pet "dog") ...) ; Zweig: (<Bedingung> <Ergebnis>)
      ((string=? pet "cat") ...)
      ((string=? pet "snake") ...))))

(define cute?
  (lambda (pet)
    (cond ; Verzweigung
      ((string=? pet "dog") #t) ; Zweig: (<Bedingung> <Ergebnis>)
      ((string=? pet "cat") #t)
      ((string=? pet "snake") #f))))

; Uhrzeit besteht aus: / hat folgende Eigenschaften:
; - Stunden - UND -
; - Minuten
; zusammengesetzte Daten
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour natural) ; Selektor
  (time-minute natural))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; 10 Uhr 52
(define time1 (make-time 10 52))

; 14 Uhr 12
(define time2 (make-time 14 12))

; Minuten seit Mitternacht berechnen
(: msm (time -> natural))

(check-expect (msm time1) 652)

; Schablone:
#;(define msm
  (lambda (time)
    ... (time-hour time) ... (time-minute time)))
              
(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

; Aus Minuten nach Mitternacht die Zeit berechnen
; (: msm->time (natural -> time))
; zusammengesetzte Daten als Ausgabe
; Schablone: Konstruktoraufruf
; (make-time ... ...)

; Tiere auf dem texanischen Highway

; Ein Tier ist eins der folgenden:
; - Gürteltier - ODER -
; - Papagei
; Fallunterscheidung
; gemischte Daten
(define animal
  (signature (mixed dillo parrot)))


; Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot - UND -
; - Gewicht
(define-record dillo
  make-dillo
  dillo? ; Prädikat
  (dillo-alive? boolean)
  (dillo-weight number))

(: dillo? (any -> boolean))


; Gürteltier, lebendig, 10kg
(define dillo1 (make-dillo #t 10))
; totes Gürteltier, 12kg
(define dillo2 (make-dillo #f 12))

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))
(check-expect (run-over-dillo dillo2)
              dillo2)

; 2 Schablonen kombiniert
#;(define run-over-dillo
  (lambda (dillo)
    ... (make-dillo ... ...) ...
    (dillo-alive? dillo) ... (dillo-weight dillo) ...))

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))

; Gürteltier füttern
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 2)
              (make-dillo #t 12))
(check-expect (feed-dillo dillo2 5) dillo2)

(define feed-dillo
  (lambda (dillo amount)
    (define alive? (dillo-alive? dillo))
    (define weight (dillo-weight dillo))
    (make-dillo alive?
                (if alive?
                    (+ weight amount)
                    weight)
                #;(cond
                  ((dillo-alive? dillo) (+ (dillo-weight dillo) amount))
                  (else (dillo-weight dillo))))))

; Ein Papagei hat folgende Eigenschaften:
; - Satz - UND -
; - Gewicht
(define-record parrot
  make-parrot parrot?
  (parrot-sentence string)
  (parrot-weight number))

; Begrüßungs-Papagei, 1kg
(define parrot1 (make-parrot "Hallo!" 1))

; Verabschiedungs-Papagei, 2kg
(define parrot2 (make-parrot "Tschüß!" 2))

; Papgei überfahren
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


#|
interface Animal {
  Animal runOver();
}

class Dillo implements Animal {
  boolean isAlive;
  double weight;
}

class Parrot implements Animal { ... }

|#


; Datenanalyse
; - ("primitive" Daten - Zahlen, Zeichenketten, Booleans)
; - Fallunterscheidungen (Aufzählungen, gemischte Daten)
; - zusammengesetzte Daten

; Eine Liste ist eins der folgenden:
; - die leere Liste
; - eine Cons-Liste, bestehend aus erstem Element und Rest-Liste
;                                                          ^^^^^  Selbstbezug
(define list-of-numbers
  (signature (mixed empty-list
                    cons-list)))

; Die leere Liste ... "Singleton"
(define-record empty-list
  make-empty-list empty?)

(define empty (make-empty-list))

; Eine Cons-Liste besteht aus:
; - erstes Element
; - Rest-Liste
(define-record cons-list
  cons cons?
  (first number)
  (rest list-of-numbers))

; einelementige Liste: 5
(define list1 (cons 5 empty))
; 2elementige Liste: 8 5
(define list2 (cons 8 (cons 5 empty)))
; 3elementige Liste: 5 9 4
(define list3 (cons 5 (cons 9 (cons 4 empty))))
; 4elementige Liste: 6 5 9 4
(define list4 (cons 6 list3))

; Elemente einer Liste addieren
(: list-sum (list-of-numbers -> number))

(check-expect (list-sum list4) 24)

; Schablone:

#;(define list-sum
  (lambda (list)
    (cond
      ((empty? list) ...)
      ((cons? list)
       ... (first list) ...
       (list-sum (rest list)) ...))))

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0)
      ((cons? list)
       (+ (first list)
          (list-sum (rest list)))))))

; Produkt der Elemente einer Liste
(: list-product (list-of-numbers -> number))

(check-expect (list-product list4) 1080)

(define list-product
  (lambda (list)
    (cond
      ((empty? list) 1)
      ((cons? list)
       (* (first list)
          (list-product (rest list)))))))

; alle geraden Zahlen einer Liste extrahieren

