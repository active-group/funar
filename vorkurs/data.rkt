#lang deinprogramm/sdp/beginner

; Datenanalyse

; Uhrzeit besteht aus/hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten
(define-record time
  make-time ; Konstruktor
  (time-hour   natural)
  (time-minute natural))

; 10 Uhr 54 Minuten
(define time1 (make-time 10 54))
(define time2 (make-time 14 32))
