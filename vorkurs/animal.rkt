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

; optional: Signaturen für Record-Teile
(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

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

; Übung:
; Minuten seit Mitternacht rein -> time raus
; Namensvorschlag: minutes-since-midnight->time
(: msm->time (natural -> time))

; hier wird strukturell auf Gleicheit geprüft
(check-expect (msm->time 743)
              time1)
(check-expect (msm->time 911)
              time2)

(define msm->time
  (lambda (minutes)
    (make-time (quotient minutes 60)
               (remainder minutes 60))))

; gemischte Daten

; Tiere auf dem texanischen Highway
; Ein Gürteltier hat folgende Eigenschaften
; - lebendig oder tot?
; - Gewicht
; -> wieder zusammengesetzte Daten

(define-record dillo
  make-dillo
  dillo? ; <- nachträglich eingefügt: Prädikat
  (dillo-alive? boolean)
  (dillo-weight natural))

(define dillo1 (make-dillo #t 10))
(define dillo2 (make-dillo #f 5))

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))
(check-expect (run-over-dillo dillo2)
              dillo2)

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f
                (dillo-weight dillo))))

; Gürteltier füttern

#;(if alive? ; Prädikat
    3 ; then-Fall
    5) ; else-Fall
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 3)
              (make-dillo #t 13))
(check-expect (feed-dillo dillo2 5)
              dillo2)

(define feed-dillo
  (lambda (dillo amount)
    #;(cond
      ((dillo-alive? dillo)
       (make-dillo #t (+ amount
                         (dillo-weight dillo))))
      (#t dillo))
    (if (dillo-alive? dillo)
        (make-dillo #t (+ amount
                         (dillo-weight dillo)))
        dillo)))

; Ein Papagei hat folgende Eigenschaften
; - ein Satz, den er sagen kann
; - ein Gewicht
(define-record parrot
  make-parrot
  parrot?
  (parrot-sentence string)
  (parrot-weight number))

; Ein Begrüßungspapagei
(define parrot1 (make-parrot "Hallo!" 2))
; Verabschiedungspapagei
(define parrot2 (make-parrot "Ciao" 1))

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot1)
              (make-parrot "" 2))
(check-expect (run-over-parrot parrot2)
              (make-parrot "" 1))

(define run-over-parrot
  (lambda (parrot)
    (make-parrot "" (parrot-weight parrot))))

; Ein Tier ist eins der Folgenden
; - Ein Gürteltier -ODER-
; - Ein Papagei
; -> gemischte Daten -> mixed
(define animal
  (signature (mixed dillo parrot)))

; Tiere überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal dillo1)
              (run-over-dillo dillo1))
(check-expect (run-over-animal dillo2)
              (run-over-dillo dillo2))
(check-expect (run-over-animal parrot1)
              (run-over-parrot parrot1))
(check-expect (run-over-animal parrot2)
              (run-over-parrot parrot2))

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (run-over-dillo animal))
      ((parrot? animal) (run-over-parrot animal)))))

#|
interface Animal { Animal runOver(); Animal petAnimal(); }

class Dillo implements Animal { @Override Animal runOver(); petAnimal  }
class Parrot implements Animal { ... }


|#
