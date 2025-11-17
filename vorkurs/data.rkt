#lang deinprogramm/sdp/beginner

; Datenanalyse

; Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; -> Fallunterscheidung, hier: Aufzählung

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