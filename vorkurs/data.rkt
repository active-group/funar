#lang deinprogramm/sdp/beginner

; Datenanalyse

; Uhrzeit besteht aus/hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour   natural) ; Selektor ("Getter-Funktion")
  (time-minute natural))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; 10 Uhr 54 Minuten
(define time1 (make-time 10 54))
(define time2 (make-time 14 32))

; Minuten seit Mitternacht
(: minutes-since-midnight (time -> natural))

(check-expect (minutes-since-midnight time1)
              654)
(check-expect (minutes-since-midnight time2)
              872)

; Gerüst
#;(define minutes-since-midnight
  (lambda (time)
    ...))

; Schablone, entsteht aus der Signatur
#;(define minutes-since-midnight
  (lambda (time)
    ; zusammengesetzte Daten als Input
    ... (time-hour time) ...
    ... (time-minute time) ...))

(define minutes-since-midnight
  (lambda (time)
    ; zusammengesetzte Daten als Input
    (+ (* 60 (time-hour time))
       (time-minute time))))

; aus Minuten-seit-Mitternacht die Uhrzeit berechnen
(: msm->time (natural -> time))

(check-expect (msm->time 654)
              (make-time 10 54))

(define msm->time
  (lambda (minutes)
    (define hour (quotient minutes 60))
    (define minute (remainder minutes 60))
    (make-time hour minute)))

; Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; Fallunterscheidung
; hier: Aufzählung
(define pet
  (signature (enum "dog"
                   "cat"
                   "snake")))

; Ist Haustier niedlich?
(: cute? (pet -> boolean))

(check-expect (cute? "dog") #t)
(check-expect (cute? "cat") #t)
(check-expect (cute? "snake") #f)

; Schablone
#;(define cute?
  (lambda (pet)
    ; Fallunterschiedung -> Verzweigung
    ; für jeden Fall einen Zweig
    ; werden sequentiell abgearbeitet
    (cond
      ; Zweig: (<Bedingung> <Ergebnis>)
      ((equal? pet "dog") ...)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))

(define cute?
  (lambda (pet)
    ; Fallunterschiedung -> Verzweigung
    ; für jeden Fall einen Zweig
    ; werden sequentiell abgearbeitet
    (cond
      ; Zweig: (<Bedingung> <Ergebnis>)
      ((equal? pet "dog") #t)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f))))
