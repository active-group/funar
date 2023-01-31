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
#;(define cute?
    (lambda (pet)
      ...))

; bei Daten, die Fallunterscheidung sind ->
; Fälle unterschiedlich behandeln
; -> cond
; Schablone:
(define cute?
  (lambda (pet)
    (cond ; Verzweigung: ein Paar pro Fall
      ((string=? pet "dog") #t) ; (<Beding> <Erg>)
      ((string=? pet "snake") #f)
      ((string=? pet "cat") #t))))

; Digitaluhr (Uhrzeit) hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten -> Record
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour natural) ; Selektoren
  (time-minute natural))

(define time1 (make-time 12 23))
(define time2 (make-time 15 11))

; Minuten seit Mitternacht ausrechnen
(: minutes-since-midnight (time -> natural))

(check-expect (minutes-since-midnight time1)
              743)
(check-expect (minutes-since-midnight time2)
              911)

; Brauchen: Schablone f. zusammengesetzte Daten
; - müssen Bestandteile anschauen
#;(define minutes-since-midnight
    (lambda (time)
      ... (time-hour time) ... (time-minute time) ...))

(define minutes-since-midnight
  (lambda (time)
    (+ (* 60
          (time-hour time))
       (time-minute time))))