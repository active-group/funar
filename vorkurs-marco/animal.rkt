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


; Minuten seit Mitternacht
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

; Aufgabe: Aus Minuten nach Mitternacht die Uhrzeit berechnen.
(: msm->time (natural -> time))

(check-expect (msm->time 649)
              time1)
(check-expect (msm->time 1435)
              time2)
(check-expect (msm->time 0)
              (make-time 0 0))

(define msm->time
  (lambda (minutes)
    (make-time (quotient minutes 60)
               (remainder minutes 60))))






; Tiere auf dem texanischen Highway sind eines der folgenden
; - ein Gürteltier - ODER -
; - ein Papagei
; gemischte Daten

(define animal
  (signature (mixed dillo parrot)))

; Ein Gürteltier besteht aus
; - lebendig oder tot? - UND -
; - Gewicht
; zusammengesetzen Daten

(define-record dillo ; signatur
  make-dillo ; konstruktor
  dillo? ; prädikat
  (dillo-alive? boolean)
  (dillo-weight number))

(: make-dillo (boolean number -> dillo))
(: dillo? (any -> boolean))

; lebendiges dillo mit 33kg
(define dillo1 (make-dillo #t 33))
(define dillo2 (make-dillo #f 15))

; Ein Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 33))
(check-expect (run-over-dillo dillo2)
              dillo2)

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f
                (dillo-weight dillo))))

; Ein Gürteltier füttern
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 5)
              (make-dillo (dillo-alive? dillo1)
                          (+ 5 (dillo-weight dillo1))))
(check-expect (feed-dillo dillo2 5)
              dillo2
              #;(make-dillo (dillo-alive? dillo2)
                          (dillo-weight dillo2)))

(define feed-dillo
  (lambda (dillo amount)
    (define alive? (dillo-alive? dillo))
    (define weight (dillo-weight dillo))
    (make-dillo alive?
                (if alive?
                    (+ weight amount)
                    weight))))
    #;(cond
      ((dillo-alive? dillo)
       (make-dillo #t
                   (+ amount (dillo-weight dillo))))
      (else dillo))

; Papagei besteht aus
; - ein Satz den er sprechen kann - UND -
; - Gewicht
; zusammengesetzte Daten
(define-record parrot
  make-parrot
  parrot? ; prädikat
  (parrot-sentence string)
  (parrot-weight number))

(define hallo (make-parrot "Hi!" 3))
(define ciao (make-parrot "Ciao!" 2))

; Einen Papagei überfahren
(: run-over-parrot (parrot -> parrot))
(check-expect (run-over-parrot hallo)
              (make-parrot ""
                           3))
(check-expect (run-over-parrot ciao)
              (make-parrot ""
                           2))

(define run-over-parrot
  (lambda (parrot)
    (make-parrot "" (parrot-weight parrot))))

; Ein Tier auf dem texanischen Highway überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal dillo1)
              (make-dillo #f 33))
(check-expect (run-over-animal hallo)
              (make-parrot ""
                           3))

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (run-over-dillo animal))
      ((parrot? animal) (run-over-parrot animal)))))

#|

interface Animal { Animal runOver(); }

class Dillo implements Animal { @Override Animal runOver(); }
class Parrot implements Animal { ... }

|#




