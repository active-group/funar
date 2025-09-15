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
  (time-hour natural)
  (time-minute natural))