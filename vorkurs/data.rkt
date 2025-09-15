#lang deinprogramm/sdp/beginner
; Datendefinition
; Haustier ist eins der folgenden:
; - Hund ODER
; - Katze ODER
; - Schlange
; Fallunterscheidung / Summe
; Aufzählung

; -> für jede Datendefinition eine Signatur
(define pet (signature (enum "dog" "cat" "snake")))

; Ist Haustier niedlich?
(: cute? (pet -> boolean))

(check-expect (cute? "dog") #t)
(check-expect (cute? "cat") #t)
(check-expect (cute? "snake") #f)

; Gerüst
#;(define cute?
  (lambda (pet)
    ...))

; Schablone
#;(define cute?
  (lambda (pet)
    ; Verzweigung
    ; 1 Zweig pro Fall
    (cond
      ; (<Bedingung>  <Ergebnis>)
      ((equal? pet "dog") ...)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))

(define cute?
  (lambda (pet)
    ; Verzweigung
    ; 1 Zweig pro Fall
    (cond
      ; (<Bedingung>  <Ergebnis>)
      ((equal? pet "dog") #t)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f))))

; Uhrzeit besteht aus / hat folgende Eigenschaften
; - Stunde UND
; - Minute
; zusammengesetzte Daten / Produkt
(define-record time  ; Signatur
  make-time ; Konstruktor
  (time-hour natural) ; Selektoren / "Getter-Funktion"
  (time-minute natural))

(define time1 (make-time 11 02))
(define time2 (make-time 14 13))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; Minuten seit Mitternacht
(: msm (time -> natural))

(check-expect (msm time1) 662)
(check-expect (msm time2) 853)

; Schablone
#;(define msm
  (lambda (time)
    ... (time-hour time) ... (time-minute time) ...))

(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

; Aus Minuten seit Mitternacht die Uhrzeit berechnen
(: msm->time (natural -> time))

(check-expect (msm->time 662) time1)

(define msm->time
  (lambda (minutes)
    (define minutes-on-day (remainder minutes (* 24 60)))
    (make-time (quotient minutes-on-day 60)
               (remainder minutes-on-day 60))))

; Tiere auf dem texanischen Highway

; Gürteltier hat folgende Eigenschaften:
; - lebendig? UND
; - Gewicht
(define-record dillo
  make-dillo
  (dillo-alive? boolean)
  (dillo-weight number))


  


; lexikalische Bindung: vom Vorkommen einer Variable innen -> außen nach Bindung suchen
; Bindung: lambda oder define