#lang deinprogramm/sdp/beginner
; Datenanalyse

; Ein Haustier ist eins folgenden:
; - Hund - ODER -
; - Katze - ODER -
; - Schlange
; Fallunterscheidung
; speziell hier: Aufzählung
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

; Schablone
#;(define cute?
  (lambda (pet)
    (cond ; Verzweigung
      ((string=? pet "dog") ...) ; Zweig: (<Bedingung> <Ergebnis>)
      ((string=? pet "cat") ...)
      ((string=? pet "snake") ...))))

(define cute?
  (lambda (pet)
    (cond ; Verzweigung
      ((string=? pet "dog") #t) ; Zweig: (<Bedingung> <Ergebnis>)
      ((string=? pet "cat") #t)
      ((string=? pet "snake") #f))))
