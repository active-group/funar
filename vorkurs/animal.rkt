#lang deinprogramm/sdp/beginner

; Datenanalyse

; Datendefinition:
; Ein Haustier ist eins der folgenden:
; - Hund - ODER -
; - Katze - ODER -
; - Schlange
; Fallunterscheidung
; hier speziell: Aufzählung
(define pet
  (signature (enum "dog" "cat" "snake")))

; Ist ein Haustier niedlich?
(: cute? (pet -> boolean))

(check-expect (cute? "dog") #t)
(check-expect (cute? "cat") #t)
(check-expect (cute? "snake") #f)

; Gerüst:
#;(define cute?
  (lambda (pet)
    ...))

; Schablone
#;(define cute?
  (lambda (pet)
    (cond ; Verzweigung, 1 Zweig pro Fall
      ((string=? pet "dog") ...) ; (<Bedingung> <Ergebnis>)
      ((string=? pet "cat") ...)
      ((string=? pet "snake") ...))))

(define cute?
  (lambda (pet)
    (cond ; Verzweigung, 1 Zweig pro Fall
      ((string=? pet "dog") #t) ; (<Bedingung> <Ergebnis>)
      ((string=? pet "cat") #t)
      ((string=? pet "snake") #f))))

; Eine Stunde ist eine natürliche Zahl zwischen 0 und 23.
(define hour (signature (integer-from-to 0 23)))
(define minute (signature (integer-from-to 0 59)))
  
; Uhrzeit besteht aus/hat folgende Eigenschaften:
; - Stunde - UND -
; - Minute
; zusammengesetzte Daten
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour hour) ; Selektor
  (time-minute minute))

(: make-time (hour minute -> time))
(: time-hour (time -> hour))
(: time-minute (time -> minute))

; 11 Uhr 23
(define time1 (make-time 11 23))
; 14:11
(define time2 (make-time 14 11))