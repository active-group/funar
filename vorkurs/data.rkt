#lang deinprogramm/sdp/beginner

; Konstruktionsanleitungen / design recipes

; Datenanalyse

; Datendefinition

; Ein Haustier ist eins der folgenden:
; - Hund - ODER -
; - Katze - ODER -
; - Schlange
; Fallunterscheidung
; hier speziell: Aufzählung

; Datendefinition -> Signatur
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

; Schablone: entsteht aus der Signatur und den Daten
#;(define cute?
  (lambda (pet)
    ; Eingabe: pet, Fallunterscheidung
    ; brauchen: Verzweigung, 1 Zweig pro Fall
    (cond
      ; jeder Zweig: (<Bedingung> <Antwort>)
      ((string=? pet "dog") ...)
      ((string=? pet "cat") ...)
      ((string=? pet "snake") ...))))

(define cute?
  (lambda (pet)
    ; Eingabe: pet, Fallunterscheidung
    ; brauchen: Verzweigung, 1 Zweig pro Fall
    (cond
      ; jeder Zweig: (<Bedingung> <Antwort>)
      ((string=? pet "dog") #t)
      ((string=? pet "cat") #t)
      ((string=? pet "snake") #f))))
