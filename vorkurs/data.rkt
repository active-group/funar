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
(define list-of-numbers
  (signature (mixed empty-list cons-list)))

(define-singleton empty-list ; Signatur
  empty ; Wert
  empty?) ; Prädikat

(define-record cons-list
  cons
  cons?
  (first number)
  (rest list-of-numbers))

; 1elementige Liste: 5
(define list1 (cons 5 empty))
; 2elementige Liste: 5 8
(define list2 (cons 5 (cons 8 empty)))
; 3elementige Liste: 2 5 8
(define list3 (cons 2 list2 #;(cons 5 (cons 8 empty))))
; 4elementige Liste: 3 2 5 8
(define list4 (cons 3 list3))

