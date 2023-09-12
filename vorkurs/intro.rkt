#lang deinprogramm/sdp/beginner

; Datenanalyse

; Datendefinition:

; Ein Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; Fallunterscheidung
; hier speziell Aufzählung
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

; Schablone
#;(define cute?
  (lambda (pet)
    (cond ; Fallunterscheidung => Verzweigung, 1 Zweig pro Fall
      ((string=? pet "dog") ...)
      ((string=? pet "cat") ...)
      ((string=? pet "snake") ...))))


(define cute?
  (lambda (pet)
    (cond ; Fallunterscheidung => Verzweigung, 1 Zweig pro Fall
      ((string=? pet "dog") #t)
      ((string=? pet "cat") #t)
      ((string=? pet "snake") #f))))

; Uhrzeit besteht aus/hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten
; OOP: Klasse, Java: auch "Record", Kotlin: "data class"
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour natural) ; Selektor / "Getter-Funktion"
  (time-minute natural))

; 12 Uhr 24 Minuten
(define time1 (make-time 12 24))
