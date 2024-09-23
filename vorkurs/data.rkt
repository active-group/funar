#lang deinprogramm/sdp/beginner
; Datenanalyse

; Datendefinition:
; Uhrzeit besteht aus:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten / compound data
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour   (integer-from-to 0 23)) ; Selektoren / Getter-Funktion
  (time-minute (integer-from-to 0 59)))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

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
