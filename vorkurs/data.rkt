#lang deinprogramm/sdp/beginner

; Datenanalyse

; Uhrzeit besteht aus/hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour   (integer-from-to 0 23)) ; Selektor ("Getter-Funktion")
  (time-minute (integer-from-to 0 59)))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; 10 Uhr 54 Minuten
(define time1 (make-time 10 54))
(define time2 (make-time 14 32))

; Minuten seit Mitternacht
(: minutes-since-midnight (time -> natural))

(check-expect (minutes-since-midnight time1)
              654)
(check-expect (minutes-since-midnight time2)
              872)

; Gerüst
#;(define minutes-since-midnight
  (lambda (time)
    ...))

; Schablone, entsteht aus der Signatur
#;(define minutes-since-midnight
  (lambda (time)
    ; zusammengesetzte Daten als Input
    ... (time-hour time) ...
    ... (time-minute time) ...))

(define minutes-since-midnight
  (lambda (time)
    ; zusammengesetzte Daten als Input
    (+ (* 60 (time-hour time))
       (time-minute time))))

; aus Minuten-seit-Mitternacht die Uhrzeit berechnen
(: msm->time (natural -> time))

(check-expect (msm->time 654)
              (make-time 10 54))

(define msm->time
  (lambda (minutes)
    (define hour (quotient minutes 60))
    (define minute (remainder minutes 60))
    (make-time hour minute)))

(check-property
 (for-all ((t time))
   (equal? t
           (msm->time (minutes-since-midnight t)))))


; Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; Fallunterscheidung
; hier: Aufzählung
(define pet
  (signature (enum "dog"
                   "cat"
                   "snake")))

; Ist Haustier niedlich?
(: cute? (pet -> boolean))

(check-expect (cute? "dog") #t)
(check-expect (cute? "cat") #t)
(check-expect (cute? "snake") #f)

; Schablone
#;(define cute?
  (lambda (pet)
    ; Fallunterschiedung -> Verzweigung
    ; für jeden Fall einen Zweig
    ; werden sequentiell abgearbeitet
    (cond
      ; Zweig: (<Bedingung> <Ergebnis>)
      ((equal? pet "dog") ...)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))

(define cute?
  (lambda (pet)
    ; Fallunterschiedung -> Verzweigung
    ; für jeden Fall einen Zweig
    ; werden sequentiell abgearbeitet
    (cond
      ; Zweig: (<Bedingung> <Ergebnis>)
      ((equal? pet "dog") #t)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f))))

; Tier auf dem texanischen Highway:
; - Gürteltier -ODER-
; - Papagei

; Gürteltier hat folgende Eigenschaften:
; - lebendig? -UND-
; - Gewicht
; eigentlich: (Repräsentation des) Zustand des Gürteltiers zu einem bestimmten Zeitpunkt
(define-record dillo
  make-dillo
  (dillo-alive? boolean)
  (dillo-weight number))

; lebendiges Gürteltier 10kg
(define dillo1 (make-dillo #t 10))
; totes Gürteltier 8kg
(define dillo2 (make-dillo #f 8))

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))
(check-expect (run-over-dillo dillo2)
              dillo2)

(check-property
 (for-all ((d dillo))
   (not (dillo-alive? (run-over-dillo d)))))

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))
  
; Gürteltier füttern (mit einer spezifizierbaren Menge Futter)
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 5)
              (make-dillo #t 15))
(check-expect (feed-dillo dillo2 5)
              dillo2)

(define feed-dillo
  (lambda (dillo amount)
    (define alive? (dillo-alive? dillo))
    (define weight (dillo-weight dillo))
    (make-dillo alive?
                ; binäre Verzweigung
                (if alive?
                    (+ weight amount)
                    weight)
                #;(cond
                  (alive?
                   (+ weight amount))
                  (else
                   #;(not (dillo-alive? dillo))
                   weight)))))


; Papagei hat folgende Eigenschaften:
; - Satz -UND-
; - Gewicht
(define-record parrot
  make-parrot
  (parrot-sentence string)
  (parrot-weight number))

; 1kg-Papagei, sagt "hello!"
(define parrot1 (make-parrot "hello!" 1))
(define parrot2 (make-parrot "goodbye!" 2))

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot1)
              (make-parrot "" 1))
              
(define run-over-parrot
  (lambda (parrot)
    (make-parrot "" (parrot-weight parrot))))
    