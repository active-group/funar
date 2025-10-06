#lang deinprogramm/sdp/beginner

; Datendefinition:
; Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; Fallunterscheidung
; hier speziell: Aufzählung / "Enumeration"

(define pet
  (signature
   (enum "dog"
         "cat"
         "snake")))

; Ist Haustier niedlich?
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

; hängt nur von Signatur ab:
; Schablone
#;(define cute?
  (lambda (pet)
    (cond ; Verzweigung
      ; ein Zweig pro Fall
      ; Format: (<Bedingung> <Ergebnis>)
      ((equal? pet "dog") ...)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))

(define cute?
  (lambda (pet)
    (cond ; Verzweigung
      ; ein Zweig pro Fall
      ; Format: (<Bedingung> <Ergebnis>)
      ((equal? pet "dog") #t)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f))))

; Uhrzeit besteht aus / hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten
(define-record time
  make-time ; Konstruktor
  ; Felder:
  (time-hour   (integer-from-to 0 23)) ; Selektor, "Getter-Funktion"
  (time-minute (integer-from-to 0 59)))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; natural: natürliche Zahlen, 0,1,2,3,4,5,6,...

; 11 Uhr 27 Minuten
(define time1 (make-time 11 27))
; 14:13
(define time2 (make-time 14 13))

; Minuten seit Mitternacht
(: msm (time -> natural))

(check-expect (msm time1)
              687)
(check-expect (msm time2)
              853)

; Schablone
#;(define msm
  (lambda (time)
    ... (time-hour time) ...
    ... (time-minute time) ...))

; NICHT: hour time-hour (time-hour)

(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

; Aus Minuten seit Mitternacht das entsprechende time-Objekt konstruieren
(: msm->time (natural -> time))

; Schablone
#;(define msm->time
  (lambda (minutes)
    (make-time ... ...)))

(check-expect
 (msm->time (msm time1))
 time1)

(check-expect
 (msm->time (msm time2))
 time2)

; Property-based testing / QuickCheck
(check-property
 (for-all ((t time))
   (equal?
    (msm->time (msm t))
    t)))

(define msm->time
  (lambda (minutes)
    (make-time (quotient minutes 60)
               (remainder minutes 60))))

; Tiere auf dem texanischen Highway

; Gürteltier hat folgende Eigenschaften:
; - lebendig? -UND-
; - Gewicht
(define-record dillo
  make-dillo
  (dillo-alive? boolean)
  (dillo-weight number))
; "Zustand des Gürteltiers zu einem bestimmten Zeitpunkt"

(: make-dillo (boolean number -> dillo))
(: dillo-alive? (dillo -> boolean))
(: dillo-weight (dillo -> number))

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
    ...
    (dillo-alive? dillo)
    (dillo-weight dillo)
    ...))

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))
