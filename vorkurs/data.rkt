#lang deinprogramm/sdp/beginner
; Datenanalyse

; Datenbeschreibung

; Ein Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; Fallunterscheidung, hier: Aufzählung

; ... -> Code
(define pet
  (signature (enum "dog" "cat" "snake")))

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

; Schablone: entsteht systematisch aus der Signatur + Datendefinitionen
#;(define cute?
  (lambda (pet)
    ; Verzweigung: was anderes machen je nach Fall
    (cond
      ; 1 Zweig pro Fall
      ((equal? pet "dog") ...) ; (<Bedingung> <Ergebnis>)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))

(define cute?
  (lambda (pet)
    ; Verzweigung: was anderes machen je nach Fall
    (cond
      ; 1 Zweig pro Fall
      ((equal? pet "dog") #t) ; (<Bedingung> <Ergebnis>)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f))))


; Uhrzeit hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour   natural) ; natural: natürliche Zahlen
  (time-minute natural)) ; Selektor, "Getter"-Funktion

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; 12 Uhr 24 Minuten
(define time1 (make-time 12 24))

; 15:03 Uhr
(define time2 (make-time 15 03))
  
; Minuten seit Mitternacht
(: minutes-since-midnight (time -> natural))

(check-expect (minutes-since-midnight time1)
              744)
(check-expect (minutes-since-midnight time2)
              903)

; Schablone
#;(define minutes-since-midnight
  (lambda (time)
    ... (time-hour time) ...
    ... (time-minute time) ...))

(define minutes-since-midnight
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

; aus Minuten-seit-Mitternacht die Uhrzeit berechnen
(: msm->time (natural -> time))

(check-expect (msm->time 744)
              time1)

; Schablone
#;(define msm->time
  (lambda (minutes)
    (make-time
     ...
     ...)))

(define msm->time
  (lambda (minutes)
    (make-time
     (quotient minutes 60)
     (remainder minutes 60))))

; Tier auf dem texanischen Highway
; - Gürteltier -ODER-
; - Schlange
; Fallunterscheidung
; gemischte Daten
(define animal
  (signature (mixed dillo snake)))

; Gürteltier hat folgende Eigenschaften:
; - lebendig? -UND-
; - Gewicht
(define-record dillo
  make-dillo
  dillo?
  (dillo-alive? boolean)
  (dillo-weight number))

; Beschreibung des Zustands des Gürteltiers zu einem bestimmten Zeitpunkt

(: make-dillo (boolean number -> dillo))
(: dillo-alive? (dillo -> boolean))
(: dillo-weight (dillo -> number))
(: dillo? (any -> boolean))

; lebendiges Gürteltier, 10kg
(define dillo1 (make-dillo #t 10))

; totes Gürteltier, 8kg
(define dillo2 (make-dillo #f 8))

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))

(check-expect (run-over-dillo dillo2)
              dillo2)

; Schablone:
#;(define run-over-dillo
  (lambda (dillo)
    (make-dillo ... ...)
    (dillo-alive? dillo)
    (dillo-weight dillo)
    ...))

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))

; Gürteltier füttern ... variable Menge
(check-expect
 (feed-dillo dillo1 10)
 (make-dillo #t 20))
(check-expect
 (feed-dillo dillo2 5)
 dillo2)

#;(define feed-dillo
  (lambda (dillo number)
    (cond
      ((dillo-alive? dillo)
       (make-dillo
        (dillo-alive? dillo)
        (+ (dillo-weight dillo) number)))
      ((not (dillo-alive? dillo))
       dillo))))

#;(define feed-dillo
  (lambda (dillo number)
    (cond
      ((dillo-alive? dillo)
       (make-dillo
        (dillo-alive? dillo)
        (+ (dillo-weight dillo) number)))
      (else ; syntaktischer Zucker
       dillo))))

; binäre Verzweigung
(define feed-dillo
  (lambda (dillo number)
    (if (dillo-alive? dillo)
        (make-dillo
         (dillo-alive? dillo)
         (+ (dillo-weight dillo) number))
        dillo)))

; Eine Schlange hat folgende Eigenschaften:
; - Länge -UND-
; - Dicke
(define-record snake
  make-snake
  snake? ; Prädikat
  (snake-length number)
  (snake-thickness number))

; Schlange 3m lang, 10cm dick
(define snake1 (make-snake 300 10))

; Schlange überfahren
(: run-over-snake (snake -> snake))

(check-expect (run-over-snake snake1)
              (make-snake 300 0))

(define run-over-snake
  (lambda (snake)
    (make-snake (snake-length snake) 0)))

; Tier überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal dillo1)
              (run-over-dillo dillo1))

(check-expect (run-over-animal snake1)
              (run-over-snake snake1))

; Schablone
#;(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) ...)
      ((snake? animal) ...))))

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (run-over-dillo animal))
      ((snake? animal) (run-over-snake animal)))))

#|
class Dillo {
  bool isAlive;
  double weight;

  void runOver() {
    this.isAlive = false;
  }
}
|#
