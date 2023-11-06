#lang deinprogramm/sdp/beginner

; Ist ein Haustier niedlich?

; Datendefinition:

; Ein Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; Fallunterscheidung
; hier speziell: Aufzählung
(define pet
  (signature (enum "dog" "cat" "snake")))

; Signaturdeklaration
(: cute? (pet -> boolean))

(check-expect (cute? "dog")
              #t)
(check-expect (cute? "cat")
              #t)
(check-expect (cute? "snake")
              #f)

; Gerüst
#;(define cute?
  (lambda (pet)
    ...))

; Schablone
#;(define cute?
  (lambda (pet)
    ; Verzweigung
    (cond
      ; 1 Zweig pro Fall, jeweils der Form
      ; (<Bedingung> <Ergebnis>)
      ((equal? pet "dog") ...)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))

(define cute?
  (lambda (pet)
    ; Verzweigung
    (cond
      ; 1 Zweig pro Fall, jeweils der Form
      ; (<Bedingung> <Ergebnis>)
      ((equal? pet "dog") #t)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f))))

; Uhrzeit besteht aus / hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten
(define-record time ; Signatur
  make-time ; Konstruktor (gibt nur einen)
  (time-hour natural) ; Selektoren / Getter-Funktionen
  (time-minute natural))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; 11 Uhr 31
(define time1 (make-time 11 31))

; 15:14
(define time2 (make-time 15 14))

; Minuten seit Mitternacht
(: msm (time -> natural))

(check-expect (msm time1)
              691)
(check-expect (msm time2)
              914)

; Schablone
#;(define msm
  (lambda (time)
    ... (time-hour time) ...
    ... (time-minute time) ...))

(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

; Aus Minuten seit Mitternacht die Zeit berechnen
(: msm->time (natural -> time))

(check-expect (msm->time 691)
              time1)
(check-expect (msm->time 914)
              time2)

(define msm->time
  (lambda (minutes)
    (make-time (quotient minutes 60)
               (remainder minutes 60))))

