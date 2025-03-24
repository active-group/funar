#lang deinprogramm/sdp/beginner

; Datenanalyse

; Datendefinition
; Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; Fallunterscheidung: Summe
; hier: Aufzählung
; -> Code
(define pet
  (signature (enum "dog" "cat" "snake")))

; Ist ein Haustier niedlich?
(: cute? (pet -> boolean))

(check-expect (cute? "cat") #t)
(check-expect (cute? "snake") #f)

; Schablone
#;(define cute?
  (lambda (pet)
    ; Verzweigung
    ; 1 Zweig pro Fall
    (cond
      ; Fall: (<Bedingung> <Ergebnis>)
      ((equal? pet "dog") ...)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))

(define cute?
  (lambda (pet)
    ; Verzweigung
    ; 1 Zweig pro Fall
    (cond
      ; Fall: (<Bedingung> <Ergebnis>)
      ((equal? pet "dog") #t)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f))))

; Uhrzeit besteht aus / hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten: Produkt
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour natural) ; Selektoren / "Getter-Funktionen"
  (time-minute natural))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; 11 Uhr 13 Minuten
(define time1 (make-time 11 13))
; 14:10
(define time2 (make-time 14 10))

; Minuten seit Mitternacht
(: msm (time -> natural))

(check-expect (msm time1)
              673)
(check-expect (msm time2)
              850)

; Schablone
#;(define msm
  (lambda (time)
    ... (time-hour time) ...
    ... (time-minute time) ...))


(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))


; msm->time

; zusammengesetzte Daten als Ausgabe:
; Konstruktor

; Tiere auf dem texanischen Highway

; Gürteltier hat folgende Eigenschaften:
; - (lebendig -ODER tot) -UND-
; - Gewicht
(define-record dillo
  make-dillo
  (dillo-alive? boolean)
  (dillo-weight number))

; "(Beschreibung von) Zustand des Gürteltiers zu
;  einem bestimmten Zeitpunkt"

(: make-dillo (boolean number -> dillo))

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

; Gürteltier füttern - Menge variabel
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 5)
              (make-dillo #t 15))
(check-expect (feed-dillo dillo2 5)
              dillo2)


(define feed-dillo
  (lambda (dillo amount)
    (make-dillo (dillo-alive? dillo)
                (cond                  
                  ((dillo-alive? dillo)
                   (+ (dillo-weight dillo) amount))
                  (else (dillo-weight dillo))))))









