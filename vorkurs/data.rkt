#lang deinprogramm/sdp/beginner

; Datendefinition:
; Haustier ist eins der folgenden:
; - Hund  -ODER-
; - Katze -ODER-
; - Schlange
; Fallunterscheidung
; Aufzählung
(define pet
  (signature (enum "dog"
                   "cat"
                   "snake")))

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
    ; Verzweigung, 1 Zweig pro Fall
    ; (<Bedingung> <Ergebnis>)
    (cond
      ((equal? pet "dog") ...)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))

(define cute?
  (lambda (pet)
    ; Verzweigung, 1 Zweig pro Fall
    ; (<Bedingung> <Ergebnis>)
    #;(match pet
      ("dog" #t)
      ("cat" #t)
      ("snake" #f))
        
    (cond
      ((equal? pet "dog") #t)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f))))

; Uhrzeit besteht aus / hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten / Produkte
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour natural) ; Selektor / "Getter"
  (time-minute natural))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; 11 Uhr 1 Minute
(define time1 (make-time 11 1))

; 14:07
(define time2 (make-time 14 07))

; Minuten seit Mitternacht
(: msm (time -> natural))

(check-expect (msm time1)
              661)
(check-expect (msm time2)
              847)

; Schablone
#;(define msm
  (lambda (time)
    ...
    (time-hour time)
    (time-minute time)
    ...))


#;(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

(define msm
  (lambda (time)
    (define hour (time-hour time))
    (define minute (time-minute time))
    (+ (* 60 hour)
       minute)))

; lexikalische Bindung:
; von einem Vorkommen einer Variable
; von INNEN nach AUSSEN
; suchen nach: dem ersten lambda, define

; Aus den Minuten-seit-Mitternacht eine Uhrzeit machen

; (: time-from-minutes (natural -> time))


; Schablone für die Erzeugung zusammengesetzter Daten:
; Konstruktor aufrufen

; Tiere auf dem texanischen Highway

; Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot?  -UND-
; - Gewicht
; zusammengesetzte Daten
(define-record dillo
  make-dillo
  (dillo-alive? boolean)
  (dillo-weight number))

; lebendiges Gürteltier, 10kg
(define dillo1 (make-dillo #t 10))
; tot, 8kg
(define dillo2 (make-dillo #f 8))

#|
class Dillo {
   bool alive;
   void setAlive(bool alive) {
      this.alive = alive;
   }
   void runOver() {
      this.alive = false;
   }
}
|#

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))

(check-expect (run-over-dillo dillo2)
              dillo2)
