#lang deinprogramm/sdp/beginner

; Datenanalyse

; Datendefinition (Kommentar)

; Haustier ist eins der folgenden:
; - Katze - ODER -
; - Hund - ODER -
; - Schlange
; Fallunterscheidung
; hier speziell: Aufzählung
(define pet
  (signature (enum "cat" "dog" "snake")))

; Ist Haustier niedlich?
(: cute? (pet -> boolean))

(check-expect (cute? "cat")
              #t)
(check-expect (cute? "dog")
              #t)
(check-expect (cute? "snake")
              #f)

; Gerüst
#;(define cute?
  (lambda (pet)
    ...))

; Schablone
#;(define cute?
  (lambda (pet)
    ; Fallunterscheidung in den Daten
    ; => in der Funktion: Verzweigung
    ; 1 Zweig pro Fall
    (cond
      ((string=? pet "cat") ...) ; (<Bedingung> <Ergebnis>)
      ((string=? pet "dog") ...)
      ((string=? pet "snake") ...))))

(define cute?
  (lambda (pet)
    ; Fallunterscheidung in den Daten
    ; => in der Funktion: Verzweigung
    ; 1 Zweig pro Fall
    (cond
      ((string=? pet "cat") #t) ; (<Bedingung> <Ergebnis>)
      ((string=? pet "dog") #t)
      ((string=? pet "snake") #f))))

; Uhrzeit hat folgende Eigenschaften:
; - Stunde - UND -
; - Minute
; zusammengesetzte Daten
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour natural) ; Selektor
  (time-minute natural))

; natural: Signatur für natürliche Zahlen / Zählzahlen

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))


; 12 Uhr 24 Minuten
(define time1 (make-time 12 24))
; 15:11
(define time2 (make-time 15 11))

; Minuten seit Mitternacht
(: msm (time -> natural))

(check-expect (msm time1)
              744)
(check-expect (msm time2)
              (+ (* 15 60) 11))

; Schablone:
#;(define msm
  (lambda (time)
    ... (time-hour time) ... (time-minute time) ...))

(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

; Tiere auf dem texanischen Highway

; Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot - UND -
; - Gewicht
; zusammengesetzte Daten
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
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))
(check-expect (run-over-dillo dillo2)
              dillo2)

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))

; Gürteltier füttern
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 2)
              (make-dillo #t 12))
(check-expect (feed-dillo dillo2 2)
              dillo2)

(define feed-dillo
  (lambda (dillo amount)
    (make-dillo (dillo-alive? dillo)
                (cond
                  ((dillo-alive? dillo) (+ (dillo-weight dillo)
                                           amount))
                  ((not (dillo-alive? dillo)) (dillo-weight dillo))))))

#;(define feed-dillo
  (lambda (dillo amount)
    ; lokale Variablen
    (define alive? (dillo-alive? dillo))
    (define weight (dillo-weight dillo))
    (make-dillo alive?
                ; binäre Verzweigung:
                ; (if <Bedingung> <Konsequente> <Alternative>)
                (if alive?
                    (+ weight amount)
                    weight))))
