#lang deinprogramm/sdp/beginner
; Datenanalyse

; Ein Haustier ist eins folgenden:
; - Hund - ODER -
; - Katze - ODER -
; - Schlange
; Fallunterscheidung
; speziell hier: Aufzählung
(define pet
  (signature (enum "dog" "cat" "snake")))

; Ist ein Haustier niedlich?
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
    (cond ; Verzweigung
      ((string=? pet "dog") ...) ; Zweig: (<Bedingung> <Ergebnis>)
      ((string=? pet "cat") ...)
      ((string=? pet "snake") ...))))

(define cute?
  (lambda (pet)
    (cond ; Verzweigung
      ((string=? pet "dog") #t) ; Zweig: (<Bedingung> <Ergebnis>)
      ((string=? pet "cat") #t)
      ((string=? pet "snake") #f))))

; Uhrzeit besteht aus: / hat folgende Eigenschaften:
; - Stunden - UND -
; - Minuten
; zusammengesetzte Daten
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour natural) ; Selektor
  (time-minute natural))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; 10 Uhr 52
(define time1 (make-time 10 52))

; 14 Uhr 12
(define time2 (make-time 14 12))

; Minuten seit Mitternacht berechnen
(: msm (time -> natural))

(check-expect (msm time1) 652)

; Schablone:
#;(define msm
  (lambda (time)
    ... (time-hour time) ... (time-minute time)))
              
(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

; Aus Minuten nach Mitternacht die Zeit berechnen
; (: msm->time (natural -> time))
; zusammengesetzte Daten als Ausgabe
; Schablone: Konstruktoraufruf
; (make-time ... ...)

; Tiere auf dem texanischen Highway

; Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot - UND -
; - Gewicht
(define-record dillo
  make-dillo
  (dillo-alive? boolean)
  (dillo-weight number))

; Gürteltier, lebendig, 10kg
(define dillo1 (make-dillo #t 10))
; totes Gürteltier, 12kg
(define dillo2 (make-dillo #f 12))

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))
(check-expect (run-over-dillo dillo2)
              dillo2)

; 2 Schablonen kombiniert
#;(define run-over-dillo
  (lambda (dillo)
    ... (make-dillo ... ...) ...
    (dillo-alive? dillo) ... (dillo-weight dillo) ...))

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))

; Gürteltier füttern
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 2)
              (make-dillo #t 12))
(check-expect (feed-dillo dillo2 5) dillo2)

(define feed-dillo
  (lambda (dillo amount)
    (define alive? (dillo-alive? dillo))
    (define weight (dillo-weight dillo))
    (make-dillo alive?
                (if alive?
                    (+ weight amount)
                    weight)
                #;(cond
                  ((dillo-alive? dillo) (+ (dillo-weight dillo) amount))
                  (else (dillo-weight dillo))))))
