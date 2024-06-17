#lang deinprogramm/sdp

; Datenanalyse

; F#: type pet = dog | cat | snake

; Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange -ODER-
; - Schwein
; Fallunterscheidung, hier speziell: Aufzählung
(define pet
  (signature (enum "dog" "cat" "snake" "pig")))

; Ist ein Haustier niedlich?
(: cute? (pet -> boolean))

(check-expect (cute? "dog") #t)
(check-expect (cute? "cat") #t)
(check-expect (cute? "snake") #f)
(check-expect (cute? "pig") #t)

; Gerüst
#;(define cute?
  (lambda (pet)
    ...))

; Schablone, ausschließlich aus der Signatur
#;(define cute?
  (lambda (pet)
    (cond ; Verzweigung, 1 Zweig pro Fall
      ((equal? pet "dog") ...) ; (<Bedingung> <Ergebnis>)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))

(define cute?
  (lambda (pet)
    (cond ; Verzweigung, 1 Zweig pro Fall
      ((equal? pet "dog") #t) ; (<Bedingung> <Ergebnis>)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f)
      ((equal? pet "pig") #t))))

; Tiere auf dem texanischen Highway

; Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot? -UND-
; - Gewicht
; zusammengesetzte Daten
(define-record dillo ; Signatur
  make-dillo ; Konstruktor
  (dillo-alive? boolean)
  (dillo-weight number))

(: make-dillo (boolean number -> dillo))
    
