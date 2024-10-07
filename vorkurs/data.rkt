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
