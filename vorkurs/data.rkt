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


#;(define cute?
  (lambda (pet)
    ; Fallunterscheidung als Eingabe
    ; brauchen: Verzweigung, ein Zweig pro Fall
    (cond
      ((string=? pet "dog") #t) ; (<Bedingung> <Antwort>)
      ((string=? pet "cat") #t)
      ((string=? pet "snake") #f))))

(define cute?
  (lambda (pet)
    (match pet
      ("dog" #t)
      ("cat" #t)
      ("snake" #f))))

; Uhrzeit besteht aus: / hat folgende Eigenschaften:
; - Stunde - UND -
; - Minute
; zusammengesetzte Daten
(define-record time ; Signatur
  make-time ; Konstruktor
  ; natural: Signatur für natürliche Zahlen: 0, 1, 2, 3, 4, 5...
  (time-hour natural) ; Selektor / "Getter-Funktion"
  (time-minute natural))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

(define time1 (make-time 10 34))
(define time2 (make-time 15 12))

; Minuten seit Mitternacht
(: msm (time -> natural))

(check-expect (msm time1) 634)
(check-expect (msm time2) 912)

; Schablone:
#;(define msm
  (lambda (time)
    ...
    (time-hour time)
    (time-minute time)
    ...
    ))


(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

; Aus Minuten seit Mitternacht die Zeit berechnen