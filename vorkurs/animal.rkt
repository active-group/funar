#lang deinprogramm/sdp/beginner
; Datenanalyse

; Ist Haustier niedlich?

; Datendefinition:
; Ein Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; Fallunterscheidung
; hier speziell: Aufzählung
(define pet
  (signature (enum "dog" "cat" "snake")))

;(: cute? ((enum "dog" "cat" "snake") -> boolean))

(: cute? (pet -> boolean))

(check-expect (cute? "dog") #t)
(check-expect (cute? "cat") #t)
(check-expect (cute? "snake") #f)
;(check-expect (cute? "mouse") #t)

; Gerüst
#;(define cute?
  (lambda (pet)
    ...))

; Schablone
#;(define cute?
  (lambda (pet)
    (cond ; Verzweigung
      ((equal? pet "dog") ...) ; (<Bedingung> <Ergebnis>)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))

(define cute?
  (lambda (pet)
    (cond ; Verzweigung
      ((equal? pet "dog") #t) ; (<Bedingung> <Ergebnis>)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f)
      #;(else #t)))) ; macht das nicht ...

; Uhrzeit besteht aus / hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour natural) ; Selektor / "Getter-Funktion"
  (time-minute natural))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; 11 Uhr 35
(define time1 (make-time 11 35))
; 14:11 Uhr
(define time2 (make-time 14 11))

; Minuten seit Mitternacht
(: msm (time -> natural))

(check-expect (msm time1)
              695)
(check-expect (msm time2)
              851)

(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

; (Minuten seit Mitternacht) rein -> Uhrzeit raus
(: msm-to-time (natural -> time))

(check-expect (msm-to-time 14)
              (make-time 0 14))

(define msm-to-time
  (lambda (minutes)
    (define hours (quotient minutes 60))
    (make-time
     hours
     (- minutes (* 60 hours))
     #;(remainder minutes 60))))


; Tiere auf dem texanischen Highway

; Gürteltiere hat folgende Eigenschaften: 
; - lebendig oder tot? -UND-
; - Gewicht
; zusammengesetzte Daten
(define-record dillo
  make-dillo
  (dillo-alive? boolean)
  (dillo-weight number))

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



