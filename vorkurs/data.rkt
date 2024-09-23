#lang deinprogramm/sdp/beginner
; Datenanalyse

; Datendefinition:
; Uhrzeit besteht aus:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten
(define-record time
  make-time ; Konstruktor
  (time-hour natural)
  (time-minute natural))

; 12 Uhr 24
(define time1 (make-time 12 24))
; 13:15
(define time2 (make-time 13 15))