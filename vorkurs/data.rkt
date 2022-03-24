#lang deinprogramm/sdp/beginner

; Konstruktionsanleitungen / design recipes

; Datenanalyse

; Datendefinition

; Ein Haustier ist eins der folgenden:
; - Hund - ODER -
; - Katze - ODER -
; - Schlange
; Fallunterscheidung
; hier speziell: Aufzählung

; Datendefinition -> Signatur
(define pet
  (signature (enum "dog" "cat" "snake")))

; f : N -> N

; Ist ein Haustier niedlich?
(: cute? (pet -> boolean))

(check-expect (cute? "dog") #t)
(check-expect (cute? "cat") #t)
(check-expect (cute? "snake") #f)

;(check-expect (cute? "parrot") #t)

; Gerüst
#;(define cute?
  (lambda (pet)
    ...))

; Schablone: entsteht aus der Signatur und den Daten
#;(define cute?
  (lambda (pet)
    ; Eingabe: pet, Fallunterscheidung
    ; brauchen: Verzweigung, 1 Zweig pro Fall
    (cond
      ; jeder Zweig: (<Bedingung> <Antwort>)
      ((string=? pet "dog") ...)
      ((string=? pet "cat") ...)
      ((string=? pet "snake") ...))))

(define cute?
  (lambda (pet)
    ; Eingabe: pet, Fallunterscheidung
    ; brauchen: Verzweigung, 1 Zweig pro Fall
    (cond
      ; jeder Zweig: (<Bedingung> <Antwort>)
      ((string=? pet "dog") #t)
      ((string=? pet "cat") #t)
      ((string=? pet "snake") #f)
      (else #t))))

; Uhrzeit besteht aus / hat folgende Eigeschaften:
; - Stunde - UND -
; - Minute
; zusammengesetzte Daten
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour natural) ; Selektor
  (time-minute natural))

; natural: natürliche Zahlen, 0,1,2,3,4,5,6,...

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; 11 Uhr 2 Minuten
(define time1 (make-time 11 2))
; 14:05 Uhr
(define time2 (make-time 14 5))

; Minuten seit Mitternacht
(: msm (time -> natural))

(check-expect (msm time1) 662)
(check-expect (msm time2) (+ (* 14 60) 5))

; Schablone:
#;(define msm
  (lambda (time)
    ... (time-hour time) ... (time-minute time) ...))

(define msm
  (lambda (time)
    (+ (* (time-hour time) 60)
       (time-minute time))))

; Minuten seit Mitternacht in time-Objekt umrechnen
(: msm->time (natural -> time))

(check-expect (msm->time 662) time1)

; Schablone

#;(define msm->time 
  (lambda (minutes)
    (make-time ... ...)))

(define msm->time 
  (lambda (minutes)
    (make-time (quotient minutes 60)
               (modulo minutes 60))))

; Tiere auf dem texanischem Highway

; Ein Tier ist eins der folgenden:
; - Gürteltier - ODER -
; - Papagei
; Fallunterscheidung (keine Aufzählung)
; gemischte Daten / "union types" / Summentypen
(define animal
  (signature
   (mixed dillo parrot)))

; Ein Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot - UND -
; - Gewicht
; zusammengesetzte Daten
(define-record dillo ; Signatur
  make-dillo ; Konstruktor
  (dillo-alive? boolean) ; Selektor
  (dillo-weight number))

(: make-dillo (boolean number -> dillo))
(: dillo-alive? (dillo -> boolean))
(: dillo-weight (dillo -> number))

; Gürteltier, lebendig, 10kg
(define dillo1 (make-dillo #t 10))
; Gürteltier, tot, 8kg
(define dillo2 (make-dillo #f 8))

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))
(check-expect (run-over-dillo dillo2)
              dillo2)

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))

; Gürteltier füttern mit bestimmtem Gewicht
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 2) (make-dillo #t 12))
(check-expect (feed-dillo dillo2 2) dillo2)

(define feed-dillo
  (lambda (dillo kg_fed)
    (cond
      ; (dillo-alive? (boolean="t" pet "dog") #t)
      ; ((boolean="f") dillo)
      ((dillo-alive? dillo)
       (make-dillo #t (+ (dillo-weight dillo) kg_fed)))
      (else dillo))))

; Ein Papagei hat folgende Eigenschaften:
; - ein Satz
; - Gewicht
(define-record parrot
  make-parrot
  (parrot-sentence string)
  (parrot-weight number))

(define parrot1 (make-parrot "Hello!" 1))
(define parrot2 (make-parrot "Goodbye!" 2))

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot1)
              (make-parrot "" 1))

(define run-over-parrot
  (lambda (parrot)
    (make-parrot "" (parrot-weight parrot))))