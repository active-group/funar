#lang deinprogramm/sdp/beginner

; Haustier _ist eins der Folgenden_:
; - Katze -ODER-
; - Hund -ODER-
; - Schlange
(define pet
  (signature (enum "cat" "dog" "snake")))

; Ist ein Haustier niedlich?
(: cute? ; <- cute-p
   (pet -> boolean))

(check-expect (cute? "dog")
              #t)
(check-expect (cute? "cat")
              #t)
(check-expect (cute? "snake")
              #f)

; Gerüst zuerst
(define cute?
  (lambda (pet)
    ...))

; bei Daten, die Fallunterscheidung sind ->
; Fälle unterschiedlich behandeln
; -> cond
; Schablone:
(define cute?
  (lambda (pet)
    (cond ; Verzweigung: ein Paar pro Fall
      ((string=? pet "dog") ...) ; (<Beding> <Erg>)
      ((string=? pet "snake") ...)
      ((string=? pet "cat") ...))))





