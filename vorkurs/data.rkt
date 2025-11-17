#lang deinprogramm/sdp/beginner

; Datenanalyse

; Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; -> Fallunterscheidung/Summe, hier: Aufzählung

; "dog", "cat", "snake"

; pro Datensorte eine Signatur
(define pet
  (signature (enum "dog" "cat" "snake")))

; Haustier niedlich?

(: cute? (pet -> boolean)) ; ? wg. boolean

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
    ; Verzweigung (<- Fallunterscheidung)
    (cond
      ; 1 Zweig pro Fall
      ; (<Bedingung> <Ergebnis>)
      ((equal? pet "dog") ...)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))

(define cute?
  (lambda (pet)
    ; Verzweigung (<- Fallunterscheidung)
    (cond
      ; 1 Zweig pro Fall
      ; (<Bedingung> <Ergebnis>)
      ((equal? pet "dog") #t)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f))))

;(cute? "gerbil")

; Uhrzeit besteht aus / hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten/Produkt

; Eine Stunde ist eine ganze Zahl zwischen 0 und 23
(define hour (signature (integer-from-to 0 23)))
; Eine Minute ist eine ganze Zahl zwischen 0 und 59
(define minute (signature (integer-from-to 0 59)))

; natural: natürliche Zahlen, 0, 1, 2, 3, 4, ...

(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour hour)
  (time-minute minute))

(: make-time (hour minute -> time))


