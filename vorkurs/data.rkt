#lang deinprogramm/sdp/beginner

; Datenanalyse

; Datendefinition
; Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; Fallunterscheidung: Summe
; hier: Aufzählung
; -> Code
(define pet
  (signature (enum "dog" "cat" "snake")))

; Ist ein Haustier niedlich?
(: cute? (pet -> boolean))

(check-expect (cute? "cat") #t)
(check-expect (cute? "snake") #f)

; Schablone
#;(define cute?
  (lambda (pet)
    ; Verzweigung
    ; 1 Zweig pro Fall
    (cond
      ; Fall: (<Bedingung> <Ergebnis>)
      ((equal? pet "dog") ...)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))

(define cute?
  (lambda (pet)
    ; Verzweigung
    ; 1 Zweig pro Fall
    (cond
      ; Fall: (<Bedingung> <Ergebnis>)
      ((equal? pet "dog") #t)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f))))

; Uhrzeit besteht aus / hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten: Produkt
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour natural)
  (time-minute natural))



