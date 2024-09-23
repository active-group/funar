#lang deinprogramm/sdp/beginner
; Datenanalyse

; Datendefinition:
; Uhrzeit besteht aus:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten / compound data
(define hour (signature (integer-from-to 0 23)))
(define minute (signature (integer-from-to 0 59)))

(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour   hour) ; Selektoren / Getter-Funktion
  (time-minute minute))

(: make-time (hour minute -> time))
(: time-hour (time -> hour))
(: time-minute (time -> minute))

; 12 Uhr 24
(define time1 (make-time 12 24))
; 13:15
(define time2 (make-time 13 15))

; Minuten seit Mitternacht
(: minutes-since-midnight (time -> natural))

(check-expect (minutes-since-midnight time1)
              744)
(check-expect (minutes-since-midnight time2)
              795)

; Gerüst
#;(define minutes-since-midnight
  (lambda (time)
    ...))

; Schablone
#;(define minutes-since-midnight
  (lambda (time)
    ... (time-hour time) (time-minute time) ...))

(define minutes-since-midnight
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

; Minuten seit Mitternacht in time-Wert umwandeln
(: msm->time (natural -> time))

(check-expect (msm->time 744)
              time1)

(check-property
 (for-all ((minutes natural))
   (expect
    (minutes-since-midnight (msm->time minutes))
    minutes)))

(check-property
 (for-all ((t time))
   (expect
    (msm->time (minutes-since-midnight t))
    t)))

(define msm->time
  (lambda (minutes)
    (make-time
     (quotient minutes 60)
     (remainder minutes 60))))

; Tiere auf dem texanischen Highway

; Tier ist eins der folgenden:
; - Gürteltier -ODER-
; - Schlange
; Fallunterscheidung, hier: gemischte Daten
(define animal
  (signature (mixed dillo
                    snake)))

; Gürteltier hat folgende Eigenschaften:
; - (lebendig -ODER- tot)   -UND-
; - Gewicht
; zusammengesetzte Daten

; Eine Liveness ist eins der folgenden:
; - lebendig -ODER-
; - tot
; Fallunterscheidung, hier: Aufzählung
(define liveness
  (signature
   (enum "alive"
         "dead")))

(define weight (signature number))

(define-record dillo
  make-dillo
  dillo? ; Prädikat
  (dillo-liveness liveness)
  (dillo-weight weight))

(: dillo? (any -> boolean))

; lebendiges Gürteltier, 10kg
(define dillo1 (make-dillo "alive" 10))
; totes Gürteltier, 8kg
(define dillo2 (make-dillo "dead" 8))

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo "dead" 10))
(check-expect (run-over-dillo dillo2)
              dillo2 ; (make-dillo "dead" 8)
              )

(define run-over-dillo
  (lambda (dillo)
    (make-dillo "dead"
                (dillo-weight dillo))))
                              
#|
class Dillo {
  Liveness liveness;
  Weight weight;

  void runOver() {
    this.liveness = Liveness.DEAD;
  }
}
|#

; Gürteltier füttern
(: feed-dillo (dillo weight -> dillo))

(check-expect (feed-dillo dillo1 2)
              (make-dillo "alive" 12))
(check-expect (feed-dillo dillo2 2)
              dillo2)

; lexikalische/statische Bindung
; vom Vorkommen von innen nach außen nach Bindung suchen
; Bindung: lambda, define, oder eingebaut

(define feed-dillo
  (lambda (dillo amount)
    (define liveness (dillo-liveness dillo))
    (define weight (dillo-weight dillo))
    (make-dillo
     liveness ; (dillo-liveness dillo)
     ; Fallunterscheidung in den Daten
     ; -> Verzweigung
     ; Pattern-Matching
     (match liveness
       ; ein Zweig pro Fall
       ("alive" (+ weight amount))
       ("dead" weight))
     #;(cond
       ; ein Zweig pro Fall
       ((equal? liveness "alive")  ; (<Bedingung> <Ergebnis>)
        (+ weight amount))
       ((equal? liveness "dead")
        weight)))))

#;(define feed-dillo
  (lambda (dillo amount)
    (match (dillo-liveness dillo)
      ("alive" (make-dillo "alive"
                           (+ (dillo-weight dillo) amount)))
      ("dead" dillo))))

; Eine Schlange hat folgende Eigenschaften:
; - Länge -UND-
; - Dicke
(define-record snake
  make-snake
  snake?
  (snake-length number)
  (snake-thickness number))

; Schlange 2m lang, 5cm dick
(define snake1 (make-snake 200 5))
; Baby-Anaconda, 5m lang, 20cm dick
(define snake2 (make-snake 500 20))

; Schlange überfahren
(: run-over-snake (snake -> snake))

(check-expect (run-over-snake snake1)
              (make-snake 200 0))
(check-expect (run-over-snake snake2)
              (make-snake 500 0))

(define run-over-snake
  (lambda (snake)
    (make-snake (snake-length snake) 0)))

; Tier überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal dillo1)
              (make-dillo "dead" 10))
(check-expect (run-over-animal snake1)
              (make-snake 200 0))

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (run-over-dillo animal))
      ((snake? animal) (run-over-snake animal)))))

; zusammengesetzte Daten:
; "besteht aus", "haben folgende Eigenschaften", "UND"

; Fallunterscheidungen
; "ist eins der folgenden", "ODER"

; häufig:
; gemischte Daten, jeder Fall zusammengesetzte Daten
; -> algebraische Datentypen

; Sums and products
; Summen (Fallunterscheidungen) und
; Produkte (zusammengesetzte Daten)

; T2: 2elementig
; T3: 3elementig
; Produkt aus T2 und T3: 6
; Summe aus T2 und T3: 5

; Eine Liste ist eins der folgenden:
; - die leere Liste -ODER-
; - eine Cons-Liste, bestehend aus erstem Element und Rest-Liste
;                                                          ^^^^^
; parametrische Polymorphie
(: list-of (signature -> signature))

(define list-of
  (lambda (element)
    (signature (mixed empty-list
                      (cons-list-of element)))))

(define-singleton empty-list ; Signatur
  empty ; Wert
  empty?) ; Prädikat

(define-record (cons-list-of element) ; macht ein lambda
  cons
  cons?
  (first element)
  (rest (list-of element)))

; 1elementige Liste: 5
(define list1 (cons 5 empty))
; 2elementige Liste: 5 8
(define list2 (cons 5 (cons 8 empty)))
; 3elementige Liste: 2 5 8
(define list3 (cons 2 list2 #;(cons 5 (cons 8 empty))))
; 4elementige Liste: 3 2 5 8
(define list4 (cons 3 list3))

(define list-of-numbers (signature (list-of number)))

; Elemente einer Liste multiplizieren
(: list-product (list-of-numbers -> number))

(check-expect (list-product list4)
              240)

#;(define list-product
  (lambda (list)
    (cond
      ((empty? list) ...)
      ((cons? list)
       (first list)
       (list-product (rest list))
       ...))))

(define list-product
  (lambda (list)
    (cond
      ((empty? list) 1) ; das NEUTRALE ELEMENT von *
      ((cons? list)
       (* (first list)
          (list-product (rest list)))))))

; neutrales Element:
; 1 * x = x * 1 = x ... von *
; 0 + x = x + 0 = x ... von +

; Algebra:
; - Menge
; - Operationen
; - Gleichungen

; Halbgruppe / Semigroup
; - Menge M
; (: op (M M -> M))
; op : M x M -> M
; Assoziativität:
; (op x (op y z)) = (op (op x y) z)

; Monoid = Halbgruppe mit neutralem Element

; Alle ungeraden Zahlen aus einer Liste extrahieren
(: extract-odds (list-of-numbers -> list-of-numbers))

(check-expect (extract-odds list4)
              (cons 3 (cons 5 empty)))

(define extract-odds
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       ; binäre Verzweigung
       (if (odd? (first list)) ; Bedingung
           (cons (first list) ; Konsequente
                 (extract-odds (rest list)))
           (extract-odds (rest list))) ; Alternative
       #;(cond
         ((odd? (first list))
          (cons (first list)
                (extract-odds (rest list))))
         (else
          (extract-odds (rest list))))))))

; abstrahieren:
; - kopieren
; - umbenennen - an die rekursiven Aufrufe denken
; - Unterschiede durch abstrakte Namen ersetzen
; - abstraken Namen in lambda aufnehmen
;   - an rekursive Aufrufe denken

; >1 Pfeil in Signatur: Funktion höherer Ordnung

; Signaturvariable: %...
(: extract
   ((%element -> boolean) (list-of %element) -> (list-of %element)))

(define extract
  (lambda (p? list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       ; binäre Verzweigung
       (if (p? (first list)) ; Bedingung
           (cons (first list) ; Konsequente
                 (extract p? (rest list)))
           (extract p? (rest list))) ; Alternative
       ))))

(define dillos (cons dillo1 (cons dillo2 empty)))

; Lebt das Gürteltier?
(: dillo-alive? (dillo -> boolean))

(check-expect (dillo-alive? dillo1) #t)
(check-expect (dillo-alive? dillo2) #f)

(define dillo-alive?
  (lambda (dillo)
    (equal? "alive" (dillo-liveness dillo))))

; Funktion auf alle Elemente einer Liste anwenden
(: list-map ((%a -> %b) (list-of %a) -> (list-of %b)))
; (: list-map ((number -> number) (list-of number) -> (list-of number))
(check-expect (list-map run-over-dillo dillos)
              (cons (make-dillo "dead" 10)
                    (cons (make-dillo "dead" 8)
                          empty)))

(define list-map
  (lambda (f list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons (f (first list))
             (list-map f (rest list)))))))