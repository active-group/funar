#lang deinprogramm/sdp/beginner

; Datendefinition:
; Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; Fallunterscheidung
; hier speziell: Aufzählung / "Enumeration"

(define pet
  (signature
   (enum "dog"
         "cat"
         "snake")))

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

; hängt nur von Signatur ab:
; Schablone
#;(define cute?
  (lambda (pet)
    (cond ; Verzweigung
      ; ein Zweig pro Fall
      ; Format: (<Bedingung> <Ergebnis>)
      ((equal? pet "dog") ...)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))

(define cute?
  (lambda (pet)
    (cond ; Verzweigung
      ; ein Zweig pro Fall
      ; Format: (<Bedingung> <Ergebnis>)
      ((equal? pet "dog") #t)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f))))

; Uhrzeit besteht aus / hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten
(define-record time
  make-time ; Konstruktor
  ; Felder:
  (time-hour   natural) ; Selektor, "Getter-Funktion"
  (time-minute natural))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; natural: natürliche Zahlen, 0,1,2,3,4,5,6,...

; 11 Uhr 27 Minuten
(define time1 (make-time 11 27))
; 14:13
(define time2 (make-time 14 13))

; Minuten seit Mitternacht
(: msm (time -> natural))

(check-expect (msm time1)
              687)
(check-expect (msm time2)
              853)

; Schablone
#;(define msm
  (lambda (time)
    ... (time-hour time) ...
    ... (time-minute time) ...))

; NICHT: hour time-hour (time-hour)

(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

; Aus Minuten seit Mitternacht das entsprechende time-Objekt konstruieren