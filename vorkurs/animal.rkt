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

; Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot? - UND -
; - Gewicht
; zusammengesetzte Daten

; eigentlich: (Wahrnehmung eines) Zustands des Gürteltiers zu einem bestimmten Zeitpunkt
(define-record dillo
  make-dillo
  (dillo-alive? boolean)
  (dillo-weight number))

(: make-dillo (boolean number -> dillo))
(: dillo-alive? (dillo -> boolean))
(: dillo-weight (dillo -> number))

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
  (parrot-sentence string)
  (parrot-weight number))

