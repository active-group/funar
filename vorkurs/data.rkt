#lang deinprogramm/sdp/beginner
; Datenanalyse
; Ein Haustier ist eins der folgenden:
; - Hund - ODER -
; - Katze - ODER -
; - Schlange
; Fallunterscheidung
; Spezialfall: Aufzählung
; Datendefinition ---> Code
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
      ; Zweig (<Bedingung> <Antwort>)
      ((string=? pet "dog") ...)
      ((string=? pet "cat") ...)
      ((string=? pet "snake") ...))))

(define cute?
  (lambda (pet)
    (cond ; Verzweigung
      ; Zweig (<Bedingung> <Antwort>)
      ((string=? pet "dog") #t)
      ((string=? pet "cat") #t)
      ((string=? pet "snake") #f))))

; Uhrzeit besteht aus / hat folgende Eigenschaften
; - Stunde - UND -
; - Minute
; zusammengesetzte Daten
(define hour
  (signature (integer-from-to 0 23)))
(define minute
  (signature (integer-from-to 0 59)))

(define-record time
  make-time ; Konstruktor
  (time-hour hour) ; Selektor / "Getter-Funktion"
  (time-minute minute))

(: make-time (hour minute -> time))
(: time-hour (time -> hour))
(: time-minute (time -> minute))


; 12 Uhr 23 Minuten
(define time1 (make-time 12 23))
; 14:40 Uhr
(define time2 (make-time 14 40))

; Minuten seit Mitternacht
(: msm (time -> natural))

(check-expect (msm time1)
              743)
(check-expect (msm time2)
              880)

; Schablone
#;(define msm
  (lambda (time)
    ... (time-hour time) (time-minute time) ...))


(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

