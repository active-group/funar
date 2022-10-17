#lang deinprogramm/sdp/beginner

; Datenanalyse

; Datendefinition (Kommentar)

; Haustier ist eins der folgenden:
; - Katze - ODER -
; - Hund - ODER -
; - Schlange
; Fallunterscheidung
; hier speziell: Aufzählung
(define pet
  (signature (enum "cat" "dog" "snake")))

; Ist Haustier niedlich?
(: cute? (pet -> boolean))

(check-expect (cute? "cat")
              #t)
(check-expect (cute? "dog")
              #t)
(check-expect (cute? "snake")
              #f)

; Gerüst
#;(define cute?
  (lambda (pet)
    ...))

; Schablone
#;(define cute?
  (lambda (pet)
    ; Fallunterscheidung in den Daten
    ; => in der Funktion: Verzweigung
    ; 1 Zweig pro Fall
    (cond
      ((string=? pet "cat") ...) ; (<Bedingung> <Ergebnis>)
      ((string=? pet "dog") ...)
      ((string=? pet "snake") ...))))

(define cute?
  (lambda (pet)
    ; Fallunterscheidung in den Daten
    ; => in der Funktion: Verzweigung
    ; 1 Zweig pro Fall
    (cond
      ((string=? pet "cat") #t) ; (<Bedingung> <Ergebnis>)
      ((string=? pet "dog") #t)
      ((string=? pet "snake") #f))))

; Uhrzeit hat folgende Eigenschaften:
; - Stunde - UND -
; - Minute
; zusammengesetzte Daten
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour natural)
  (time-minute natural))

; natural: Signatur für natürliche Zahlen / Zählzahlen