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
  (dillo-liveness liveness)
  (dillo-weight weight))

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

#;(define feed-dillo
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

(define feed-dillo
  (lambda (dillo amount)
    (match (dillo-liveness dillo)
      ("alive" (make-dillo "alive"
                           (+ (dillo-weight dillo) amount)))
      ("dead" dillo))))