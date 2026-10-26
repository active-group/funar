#lang deinprogramm/sdp/beginner
; Datenanalyse

; Haustier ist eins der folgenden:
; - Hund      ODER
; - Katze     ODER
; - Schlange
; Fallunterscheidung, hier: Aufzählung

(define pet
  (signature (enum "dog"
                   "cat"
                   "snake")))

; Ist Haustier niedlich?
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
    ; Verzweigung:
    ; 1 Zweig pro Fall (<Bedingung> <Ergebnis>)
    (cond
      ((equal? pet "dog") ...)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))

(define cute?
  (lambda (pet)
    ; Verzweigung:
    ; 1 Zweig pro Fall (<Bedingung> <Ergebnis>)
    (cond
      ((equal? pet "dog") #t)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f))))

; Uhrzeit besteht aus / hat folgende Eigenschaften:
; - Stunde  UND
; - Minute
; zusammengesetzte Daten / Produkt
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour natural) ; Selektor, "Getter-Funktion"
  (time-minute natural))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; 11 Uhr 4 Minuten
(define time1 (make-time 11 04))
; 15:13 Uhr
(define time2 (make-time 15 13))

; Minuten seit Mitternacht
(: minutes-since-midnight (time -> natural))

(check-expect (minutes-since-midnight time1)
              664)
(check-expect (minutes-since-midnight time2)
              913)

; Schablone
#;(define minutes-since-midnight
  (lambda (time)
    ... (time-hour time) ...
    ... (time-minute time) ...))


(define minutes-since-midnight
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

; Übungsaufgabe:
; Aus Minuten-seit-Mitternacht die Uhrzeit berechnen

(: msm-to-time (natural -> time))

(check-expect (msm-to-time 664) time1)

; Schablone

#;(define msm-to-time
  (lambda (minutes)
    (make-time ... ...)))

(define msm-to-time
  (lambda (minutes)
    (make-time 
     (quotient minutes 60)
     (remainder minutes 60))))

; Tiere auf dem texanischen Highway

; Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot  -UND-
; - Gewicht
(define-record dillo
  make-dillo
  (dillo-alive? boolean)
  (dillo-weight number))

(: make-dillo (boolean number -> dillo))
(: dillo-alive? (dillo -> boolean))
(: dillo-weight (dillo -> number))

; lebendiges Gürteltier, 10kg
(define dillo1 (make-dillo #t 10))

; totes Gürteltier, 8k
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
    

#| OO
class Dillo {
  bool isAlive;

  void runOver() {
     this.isAlive = false;
  }

|#
