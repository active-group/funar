#lang deinprogramm/sdp/beginner
; Datenanalyse
; Ein Haustier ist eins der folgenden:
; - Hund - ODER -
; - Katze - ODER -
; - Schlange
; Fallunterscheidung
; Spezialfall: Aufzählung
; Datendefinition ---> Code
(define pet
  (signature (enum "dog" "cat" "snake")))


; Ist ein Haustier niedlich?
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
    (cond ; Verzweigung
      ; Zweig (<Bedingung> <Antwort>)
      ((string=? pet "dog") ...)
      ((string=? pet "cat") ...)
      ((string=? pet "snake") ...))))

(define cute?
  (lambda (pet)
    (cond ; Verzweigung
      ; Zweig (<Bedingung> <Antwort>)
      ((string=? pet "dog") #t)
      ((string=? pet "cat") #t)
      ((string=? pet "snake") #f))))

; Uhrzeit besteht aus / hat folgende Eigenschaften
; - Stunde - UND -
; - Minute
; zusammengesetzte Daten
(define-record time
  make-time
  (time-hour natural)
  (time-minute natural))

