#lang deinprogramm/sdp/beginner
; "Alles ist Daten."

; Datenanalyse

; Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; Fallunterscheidung
; hier: Aufzählung

; "dog", "cat", "snake"
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

; Schablone (ergibt sich ausschließlich aus der Signatur)
#;(define cute?
  (lambda (pet)
    ; Verzweigung
    ; einen Zweig pro Fall
    ; jeder Fall hat die Form (<Bedingung> <Ergebnis>)
    (cond
      ((equal? pet "dog") ...)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))

(define cute?
  (lambda (pet)
    ; Verzweigung
    ; einen Zweig pro Fall
    ; jeder Fall hat die Form (<Bedingung> <Ergebnis>)
    (cond
      ((equal? pet "dog") #t)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f))))