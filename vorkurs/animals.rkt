#lang deinprogramm/sdp

; Datenanalyse

; F#: type pet = dog | cat | snake

; Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; Fallunterscheidung, hier speziell: Aufzählung
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

(define cute?
  (lambda (pet)
    (cond ; Verzweigung, 1 Zweig pro Fall
      ((equal? pet "dog") ...) ; (<Bedingung> <Ergebnis>)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))
