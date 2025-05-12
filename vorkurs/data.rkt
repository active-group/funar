#lang deinprogramm/sdp/beginner
; Datenanalyse

; Datenbeschreibung

; Ein Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; Fallunterscheidung, hier: Aufzählung

; ... -> Code
(define pet
  (signature (enum "dog" "cat" "snake")))

; Ist Haustier niedlich?
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

; Schablone: entsteht systematisch aus der Signatur + Datendefinitionen
#;(define cute?
  (lambda (pet)
    ; Verzweigung: was anderes machen je nach Fall
    (cond
      ; 1 Zweig pro Fall
      ((equal? pet "dog") ...) ; (<Bedingung> <Ergebnis>)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))

(define cute?
  (lambda (pet)
    ; Verzweigung: was anderes machen je nach Fall
    (cond
      ; 1 Zweig pro Fall
      ((equal? pet "dog") #t) ; (<Bedingung> <Ergebnis>)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f))))