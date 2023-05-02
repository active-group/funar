#lang deinprogramm/sdp/beginner
; Datenanalyse

; Datendefinition

; Ein Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; -> Fallunterscheidung
; hier: Aufzählung ausreichend, "dog", "cat", "snake"

; Datendefinition -> Code, Signatur
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
; pet: Fallunterscheidung
#;(define cute?
  (lambda (pet)
    ; Verzweigung, 1 Zweig pro Fall
    ; jeder Zweig: (<Bedingung> <Ergebnis>)
    (cond
      ((string=? pet "dog") ...)
      ((string=? pet "cat") ...)
      ((string=? pet "snake") ...))))

#;(define cute?
  (lambda (pet)
    ; Verzweigung, 1 Zweig pro Fall
    ; jeder Zweig: (<Bedingung> <Ergebnis>)
    (cond
      ((string=? pet "dog") #t)
      ((string=? pet "cat") #t)
      ((string=? pet "snake") #f))))

(define cute?
  (lambda (pet)
    (match pet
      ("dog" #t)
      ("cat" #t)
      ("snake" #f))))

; Eine Uhrzeit besteht aus / hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten
; Record-Definition
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour ; Selektor
   natural) ; Signatur, natürliche Zahlen 
  (time-minute natural))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; 12 Uhr 24
(define time1 (make-time 12 24))
; 14:12 Uhr
(define time2 (make-time 14 12))

; Minuten seit Mitternacht berechnen
(: minutes-since-midnight (time -> natural))

(check-expect (minutes-since-midnight time1)
              744)
(check-expect (minutes-since-midnight time2)
              852)

; Schablone
#;(define minutes-since-midnight
  (lambda (time)
    ...
    (time-hour time)
    (time-minute time)
    ...))

(define minutes-since-midnight
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

(: create-time-from-minutes (natural -> time))

(check-expect (create-time-from-minutes 744)
              time1)

(define create-time-from-minutes
  (lambda (min)
    (make-time (quotient min 60) (remainder min 60))))

; Ein Tier auf dem texanischen Highway ist eins der folgenden:
; - Gürteltier -ODER-
; - Papagei
; gemischte Daten
(define animal
  (signature (mixed dillo
                    parrot)))

; Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot -UND-
; - Gewicht
; zusammengesetzte Daten
(define-record dillo
  make-dillo
  dillo? ; Prädikat
  (dillo-alive? boolean)
  (dillo-weight number))

(: dillo? (any -> boolean))

(: make-dillo (boolean number -> dillo))
(: dillo-alive? (dillo -> boolean))
(: dillo-weight (dillo -> number))

; Gürteltier, lebendig, 10kg
(define dillo1 (make-dillo #t 10))
; totes Gürteltier, 8kg
(define dillo2 (make-dillo #f 8))

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))
(check-expect (run-over-dillo dillo2)
              dillo2)

; Schablone
#;(define run-over-dillo
  (lambda (dillo)
    (make-dillo ... ...)
    ...
    (dillo-alive? dillo)
    (dillo-weight dillo)
    ...))

#;(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))

(define run-over-dillo
  (lambda (dillo)
    (match dillo
      ((make-dillo #t w) (make-dillo #f w))
      ((make-dillo #f w) dillo))))


(define feed-dillo
  (lambda (dillo w)
    (if (dillo-alive? dillo)
        (make-dillo #t (+ w (dillo-weight dillo)))
        dillo)))

; Papagei hat folgende Eigenschaften:
; - Satz -UND-
; - Gewicht
(define-record parrot
  make-parrot
  parrot?
  (parrot-sentence string)
  (parrot-weight number))

; Begrüßungs-Papagei
(define parrot1 (make-parrot "Welcome!" 1))
; Rausschmeißer
(define parrot2 (make-parrot "Goodbye!" 2))

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot1)
              (make-parrot "" 1))
(check-expect (run-over-parrot parrot2)
              (make-parrot "" 2))

(define run-over-parrot
  (lambda (parrot)
    (make-parrot "" (parrot-weight parrot))))

; Tier überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal parrot1)
              (run-over-parrot parrot1))
(check-expect (run-over-animal dillo1)
              (run-over-dillo dillo1))

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (run-over-dillo animal))
      ((parrot? animal) (run-over-parrot animal)))))
