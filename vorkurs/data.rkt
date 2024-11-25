#lang deinprogramm/sdp/beginner
; "Alles ist Daten."

; Datenanalyse

; Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; Fallunterscheidung
; hier: Aufzählung

; "dog", "cat", "snake"
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

; Schablone (ergibt sich ausschließlich aus der Signatur)
#;(define cute?
  (lambda (pet)
    ; Verzweigung
    ; einen Zweig pro Fall
    ; jeder Fall hat die Form (<Bedingung> <Ergebnis>)
    (cond
      ((equal? pet "dog") ...)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))

(define cute?
  (lambda (pet)
    ; Verzweigung
    ; einen Zweig pro Fall
    ; jeder Fall hat die Form (<Bedingung> <Ergebnis>)
    (cond
      ((equal? pet "dog") #t)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f))))


; Uhrzeit besteht aus / hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour (integer-from-to 0 23)) ; Selektor
  (time-minute (integer-from-to 0 59)))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; 11 Uhr 38 Minuten
(define time1 (make-time 11 38))

; 14:12
(define time2 (make-time 14 12))

; Minuten seit Mitternacht
(: msm (time -> natural))

(check-expect (msm time1)
              698)
(check-expect (msm time2)
              852)

; Schablone
#;(define msm
  (lambda (time)
    ... (time-hour time) ...
    ... (time-minute time) ...))

(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))


; Aus den Minuten seit Mitternacht die Uhrzeit berechnen
; msm->time
(: msm->time (natural -> time))

(check-expect (msm->time 698)
              time1)

; property-based testing
; QuickCheck
(check-property
 (for-all ((t time))
   (equal? (msm->time (msm t))
           t)))

; Schablone

#;(define msm->time
  (lambda (minutes)
    (make-time ... ...)))


(define msm->time
  (lambda (minutes)
    (make-time (quotient minutes 60)
               (modulo minutes 60))))


; Tiere auf dem texanischem Highway

; Gürteltier hat folgende Eigenschaften:
; - lebendig? -UND-
; - Gewicht
(define-record dillo
  make-dillo
  (dillo-alive? boolean)
  (dillo-weight number))

(: make-dillo (boolean number -> dillo))
(: dillo-alive? (dillo -> boolean))
(: dillo-weight (dillo -> number))

; lebendiges Gürteltier, 10kg
(define dillo1 (make-dillo #t 10))
; totes Gürteltier, 8kg
(define dillo2 (make-dillo #f 8))

; Gürteltier überfahren
(: roadkill-dillo (dillo -> dillo))

(check-expect (roadkill-dillo dillo1)
              (make-dillo #f 10))
(check-expect (roadkill-dillo dillo2)
              dillo2)

(define roadkill-dillo
  (lambda (dillo)
    (make-dillo #f
                (dillo-weight dillo))))

; Gürteltier füttern - tote Tiere fressen nix
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 2)
              (make-dillo #t 12))
(check-expect (feed-dillo dillo2 2)
              dillo2)

(define feed-dillo
  (lambda (dillo amount)
    (make-dillo
     (dillo-alive? dillo)
     (cond
       ((equal? (dillo-alive? dillo) #t)
        (+ (dillo-weight dillo) amount))
       ((equal? (dillo-alive? dillo) #f)
        (dillo-weight dillo))))))

   

#|
class Dillo {
   void roadkill() { ... }
}
|#













