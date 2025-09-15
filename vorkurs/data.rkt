#lang deinprogramm/sdp/beginner
; Datendefinition
; Haustier ist eins der folgenden:
; - Hund ODER
; - Katze ODER
; - Schlange
; Fallunterscheidung / Summe
; Aufzählung

; -> für jede Datendefinition eine Signatur
(define pet (signature (enum "dog" "cat" "snake")))

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
    ; Verzweigung
    ; 1 Zweig pro Fall
    (cond
      ; (<Bedingung>  <Ergebnis>)
      ((equal? pet "dog") ...)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))

(define cute?
  (lambda (pet)
    ; Verzweigung
    ; 1 Zweig pro Fall
    (cond
      ; (<Bedingung>  <Ergebnis>)
      ((equal? pet "dog") #t)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f))))

; Uhrzeit besteht aus / hat folgende Eigenschaften
; - Stunde UND
; - Minute
; zusammengesetzte Daten / Produkt
(define-record time  ; Signatur
  make-time ; Konstruktor
  (time-hour natural) ; Selektoren / "Getter-Funktion"
  (time-minute natural))

(define time1 (make-time 11 02))
(define time2 (make-time 14 13))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; Minuten seit Mitternacht
(: msm (time -> natural))

(check-expect (msm time1) 662)
(check-expect (msm time2) 853)

; Schablone
#;(define msm
  (lambda (time)
    ... (time-hour time) ... (time-minute time) ...))

(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))


; lexikalische Bindung: vom Vorkommen einer Variable innen -> außen nach Bindung suchen
; Bindung: lambda oder define

; Aus Minuten seit Mitternacht die Uhrzeit berechnen
(: msm->time (natural -> time))

(check-expect (msm->time 662) time1)

(define msm->time
  (lambda (minutes)
    (define minutes-on-day (remainder minutes (* 24 60)))
    (make-time (quotient minutes-on-day 60)
               (remainder minutes-on-day 60))))

; Tiere auf dem texanischen Highway

; Tier ist eins der folgenden:
; - Gürteltier ODER
; - Papagei

; Gürteltier hat folgende Eigenschaften:
; - lebendig? UND
; - Gewicht
(define-record dillo
  make-dillo
  (dillo-alive? boolean)
  (dillo-weight number))

(: make-dillo (boolean number -> dillo))

; lebendiges Gürteltier, 10kg
(define dillo1 (make-dillo #t 10))

; totes Gürteltier, 8kg
(define dillo2 (make-dillo #f 8))

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

; "Repräsentation des Zustands des Gürteltiers zu einem bestimmten Zeitpunkt"

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))
(check-expect (run-over-dillo dillo2)
              dillo2)

(check-property
 (for-all ((d dillo))
   (not (dillo-alive? (run-over-dillo d)))))

(define run-over-dillo
  (lambda (dillo)
    (if (>= (dillo-weight dillo) 10)
        (make-dillo #f (dillo-weight dillo))
        dillo)))

; Gürteltier füttern
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 5)
              (make-dillo #t 15))
(check-expect (feed-dillo dillo2 5)
              dillo2)

#;(define feed-dillo
  (lambda (dillo amount)
    (make-dillo (dillo-alive? dillo)
                (cond
                  ((equal? (dillo-alive? dillo) #t) (+ (dillo-weight dillo) amount))
                  ((equal? (dillo-alive? dillo) #f) (dillo-weight dillo))))))

#;(define feed-dillo
  (lambda (dillo amount)
    (define alive? (dillo-alive? dillo))
    (define weight (dillo-weight dillo))
    (make-dillo alive?
                ; binäre Verzweigung
                (if alive?
                    (+ weight amount)
                    weight)
                #;(cond
                  (alive? (+ weight amount))
                  (else weight)))))

(define feed-dillo
  (lambda (dillo amount)
    (if (dillo-alive? dillo)
        (make-dillo #t (+ (dillo-weight dillo) amount))
        dillo #;(make-dillo (dillo-alive? dillo) (dillo-weight dillo)))))

; Ein Papagei hat folgende Eigenschaften:
; - Satz UND
; - Gewicht
(define-record parrot
  make-parrot
  (parrot-sentence string)
  (parrot-weight number))

(define parrot1 (make-parrot "Hallo!" 1))
(define parrot2 (make-parrot "Tschüss!" 2))

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot1) (make-parrot "" 1))
(check-expect (run-over-parrot parrot2) (make-parrot "" 2))

(define run-over-parrot
  (lambda (parrot)
    (make-parrot "" (parrot-weight parrot))))
    
              