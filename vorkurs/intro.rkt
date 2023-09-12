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

; lexikalische/statische Bindung
; von der Benutzung einer Variable von innen nach außen suchen
; die erste Definition/Bindung (lambda / define) ist zuständig.

; Uhrzeit besteht aus/hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten
; OOP: Klasse, Java: auch "Record", Kotlin: "data class"

; Eine Stunde ist eine ganze Zahl zwischen 0 und 23
(define hour (signature (integer-from-to 0 23)))
; Eine Minute ist eine ganze Zahl zwischen 0 und 59
(define minute (signature (integer-from-to 0 59)))
  
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour hour) ; Selektor / "Getter-Funktion"
  (time-minute minute))

(: make-time (hour minute -> time))
(: time-hour (time -> hour))
(: time-minute (time -> minute))


; 12 Uhr 24 Minuten
(define time1 (make-time 12 24))
; 14:32
(define time2 (make-time 14 32))

; Minuten seit Mitternacht
(: msm (time -> natural))

(check-expect (msm time1) 744)
(check-expect (msm time2) 872)

; Schablone:
#;(define msm
  (lambda (time)
    ... (time-hour time) ...
    ... (time-minute time) ...))

(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

; (natural -> time)
                          
