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


; Uhrzeit hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour   natural) ; natural: natürliche Zahlen
  (time-minute natural)) ; Selektor, "Getter"-Funktion

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; 12 Uhr 24 Minuten
(define time1 (make-time 12 24))

; 15:03 Uhr
(define time2 (make-time 15 03))
  
; Minuten seit Mitternacht
(: minutes-since-midnight (time -> natural))

(check-expect (minutes-since-midnight time1)
              744)
(check-expect (minutes-since-midnight time2)
              903)

; Schablone
#;(define minutes-since-midnight
  (lambda (time)
    ... (time-hour time) ...
    ... (time-minute time) ...))

(define minutes-since-midnight
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))


