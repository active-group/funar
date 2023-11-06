#lang deinprogramm/sdp/beginner

; Ist ein Haustier niedlich?

; Datendefinition:

; Ein Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; Fallunterscheidung
; hier speziell: Aufzählung
(define pet
  (signature (enum "dog" "cat" "snake")))

; Signaturdeklaration
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

; Schablone
(define cute?
  (lambda (pet)
    ; Verzweigung
    (cond
      ; 1 Zweig pro Fall, jeweils der Form
      ; (<Bedingung> <Ergebnis>)
      ((equal? pet "dog") ...)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))