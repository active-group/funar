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
