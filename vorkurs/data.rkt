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

