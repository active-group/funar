#lang deinprogramm/sdp/beginner
; Datenanalyse

; Datendefinition

; Ein Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; -> Fallunterscheidung
; hier: Aufzählung ausreichend, "dog", "cat", "snake"

; Datendefinition -> Code, Signatur
(define pet
  (signature (enum "dog" "cat" "snake")))


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
; pet: Fallunterscheidung
#;(define cute?
  (lambda (pet)
    ; Verzweigung, 1 Zweig pro Fall
    ; jeder Zweig: (<Bedingung> <Ergebnis>)
    (cond
      ((string=? pet "dog") ...)
      ((string=? pet "cat") ...)
      ((string=? pet "snake") ...))))

#;(define cute?
  (lambda (pet)
    ; Verzweigung, 1 Zweig pro Fall
    ; jeder Zweig: (<Bedingung> <Ergebnis>)
    (cond
      ((string=? pet "dog") #t)
      ((string=? pet "cat") #t)
      ((string=? pet "snake") #f))))

(define cute?
  (lambda (pet)
    (match pet
      ("dog" #t)
      ("cat" #t)
      ("snake" #f))))

; Eine Uhrzeit besteht aus / hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten
; Record-Definition
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour ; Selektor
   natural) ; Signatur, natürliche Zahlen 
  (time-minute natural))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; 12 Uhr 24
(define time1 (make-time 12 24))
; 14:12 Uhr
(define time2 (make-time 14 12))

; Minuten seit Mitternacht berechnen
(: minutes-since-midnight (time -> natural))

(check-expect (minutes-since-midnight time1)
              744)
(check-expect (minutes-since-midnight time2)
              852)

; Schablone
#;(define minutes-since-midnight
  (lambda (time)
    ...
    (time-hour time)
    (time-minute time)
    ...))

(define minutes-since-midnight
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))
