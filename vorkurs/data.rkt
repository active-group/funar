#lang deinprogramm/sdp/beginner

; Datenanalyse

; Haustier ist eins der folgenden:
; - Hund - ODER -
; - Katze - ODER -
; - Schlange
; Fallunterscheidung
; hier speziell: Aufzählung
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

; Schablone (ergibt sich *nur* aus der Signatur)
#;(define cute?
  (lambda (pet)
    ; Fallunterscheidung als Eingabe
    ; brauchen: Verzweigung, ein Zweig pro Fall
    (cond
      ((string=? pet "dog") ...) ; (<Bedingung> <Antwort>)
      ((string=? pet "cat") ...)
      ((string=? pet "snake") ...))))


(define cute?
  (lambda (pet)
    ; Fallunterscheidung als Eingabe
    ; brauchen: Verzweigung, ein Zweig pro Fall
    (cond
      ((string=? pet "dog") #t) ; (<Bedingung> <Antwort>)
      ((string=? pet "cat") #t)
      ((string=? pet "snake") #f))))
