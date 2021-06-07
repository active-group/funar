;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname intro) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
; Datenanalyse
; Datendefinition:
; Ein Haustier ist eins der folgenden:
; - Hund ODER
; - Katze ODER
; - Schlange
; Fallunterscheidung
; hier Spezialfall: Aufzählung
; in Code:
(define pet
  (signature
   (enum "dog" "cat" "snake")))

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
    (cond ; Verzweigung: 1 "Zeile"/Zweig pro Fall
      ; Jeder Zweig: Bedingung Ergebnis
      ((string=? pet "dog") ...)
      ((string=? pet "cat") ...)
      ((string=? pet "snake") ...)
      )))

(define cute?
  (lambda (pet)
    (cond ; Verzweigung: 1 "Zeile"/Zweig pro Fall
      ; Jeder Zweig: Bedingung Ergebnis
      ((string=? pet "dog") #t)
      ((string=? pet "cat") #t)
      ((string=? pet "snake") #f))))

;(cute? "parakeet")

(define hour (signature (integer-from-to 0 23)))
(define minute (signature (integer-from-to 0 59)))

; Uhrzeit besteht aus: / hat folgende Eigenschaften:
; - Stunde UND
; - Minute
; zusammengesetzte Daten
(define-record time ; Signatur
  make-time ; Konstruktor, "make-" Konvention
  (time-hour hour) ; Selektoren / "Getter-Funktion"
  (time-minute minute))

(: make-time (hour minute -> time))
(: time-hour (time -> hour))
(: time-minute (time -> minute))

(define time1 (make-time 12 24)) ; 12 Uhr 24
(define time2 (make-time 0 0)) ; Mitternacht

; Minuten seit Mitternacht
(: msm (time -> natural))

(check-expect (msm time1) (+ (* 12 60) 24))
(check-expect (msm time2) 0)

(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

; Andersrum
(: msm->time (natural -> time))

(check-expect (msm->time 744) time1)
(check-expect (msm->time (msm time1)) time1)

(check-property
 (for-all ((a number)
           (b number))
   (= (+ a b) (+ b a))))

(check-property
 (for-all ((t time))
   (expect (msm->time (msm t))
           t)))

(define msm->time
  (lambda (minutes)
    (make-time (quotient minutes 60)
               (remainder minutes 60))))

; Ein Gürteltier hat folgende Eigenschaften:
; - tot oder lebendig
; - Gewicht
; Zustand eines Gürteltiers zu einem bestimmten Zeitpunkt
(define-record dillo
  make-dillo
  (dillo-alive? boolean)
  (dillo-weight number))

(: make-dillo (boolean number -> dillo))

(define dillo1 (make-dillo #t 10)) ; Gürteltier, 10kg, lebendig
(define dillo2 (make-dillo #f 12)) ; totes Gürteltier, 12kg

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))
(check-expect (run-over-dillo dillo2)
              dillo2)

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))

