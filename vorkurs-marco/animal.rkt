#lang deinprogramm/sdp/beginner

; hunde, katzen, schlangen

; Datendefinition
; Ein Haustier ist eines der folgenden
; - Hund - ODER -
; - Katze - ODER -
; - Schlange - ODER -
; - Fruchtfliege
; Fallunterscheidung
; speziell hier: Aufzählung
(define pet
  (signature (enum "dog" "cat" "snake" "fruit fly")))

; Konstruktionsanleitung
; 1. Kurzbeschreibung schreiben
; 2. Signatur
; 3. Test
; 4. Gerüst

; Ist ein Haustier niedlich?
(: cute? (pet -> boolean))
#;(: cute? ((enum "dog" "cat" "snake" "fruit fly") -> boolean))

(check-expect (cute? "dog") #t)
(check-expect (cute? "cat") #t)
(check-expect (cute? "snake") #t)
(check-expect (cute? "fruit fly") #f)

(define cute?
  (lambda (animal)
    (cond
      ((string=? "dog" animal) #t)
      ((string=? "cat" animal) #t)
      ((string=? "snake" animal) #t)
      ((string=? "fruit fly" animal) #f))))

; Schablone für gemischte Daten/Falluntescheidung

#;(define XXX
    (lambda (l)
      (cond
        (fall1 konsequente1)
        (fall2 konsequente2))))


; Uhrzeiten
(define hour (signature (integer-from-to 0 23)))
(define minute (signature (integer-from-to 0 59)))

; Eine Uhrzeite besteht aus
; - Stunde - UND -
; - Minuten
; zusammengesetzte Daten
(define-record time  ; <- signatur
  make-time ; <- Konstruktor
  (time-hour hour) ; <- Selektoren
  (time-minute minute))

(: make-time (hour minute -> time))
(: time-hour (time -> hour))
(: time-minute (time -> minute))

; 10:49
(define time1 (make-time 10 49))
; fünf vor 12
(define time2 (make-time 23 55))


; Minuten seit Mitternach
(: since-midnight (time -> natural))

(check-expect (since-midnight time1)
              649)
(check-expect (since-midnight time2)
              1435)
(check-expect (since-midnight (make-time 0 0))
              0)

(define since-midnight
  (lambda (time)
    (+ (* (time-hour time)
          60)
       (time-minute time))))






