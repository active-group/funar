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
  dillo? ; Prädikat
  (dillo-alive? boolean) ; "code smell"
  (dillo-weight number)) ; auch

(: make-dillo (boolean number -> dillo))
(: dillo-alive? (dillo -> boolean))
(: dillo-weight (dillo -> number))
(: dillo? (any -> boolean))

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

; Gürteltier füttern
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 5)
              (make-dillo #t 15))
(check-expect (feed-dillo dillo2 2)
              dillo2)

(define feed-dillo
  (lambda (d amount)
    (make-dillo (dillo-alive? d)
                (if (dillo-alive? d) ; Bedingung
                    (+ (dillo-weight d) amount) ; Konsequente 
                    (dillo-weight d))))) ; Alternative

; Eine Klapperschlange hat folgende Eigenschaften:
; - Länge
; - Dicke
(define-record snake
  make-snake
  snake?
  (snake-length number)
  (snake-thickness number))

(define snake1 (make-snake 300 10)) ; 300cm lang, 10cm dick
(define snake2 (make-snake 200 5)) ; 2m lang, 5cm dick

; Klapperschlange überfahren
(: run-over-snake (snake -> snake))

(check-expect (run-over-snake snake1)
              (make-snake 300 0))

(define run-over-snake
  (lambda (snake)
    (make-snake (snake-length snake) 0)))


; Ein Tier ist eins der folgenden:
; - Gürteltier
; - Klapperschlange
; Fallunterscheidung, jeder Fall hat eine eigene Signatur
; gemischte Daten
(define animal
  (signature (mixed dillo snake)))

; Tier überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal dillo1)
              (run-over-dillo dillo1))
(check-expect (run-over-animal snake1)
              (run-over-snake snake1))

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (run-over-dillo animal))
      ((snake? animal) (run-over-snake animal)))))


; FP: Liste hat feste Bedeutung
; Eine Liste ist eins der folgenden:
; - die leere Liste
; - eine Cons-Liste bestehend aus erstem Element und Liste der restlichen Elemente
;                                                    ^^^^^ Selbstreferenz

; Eine leere Liste ist ... leer, besteht aus ... nichts
(define-record empty-list
  make-empty-list
  empty?)

(define empty (make-empty-list))

; Eine Cons-Liste besteht aus:
; - erstes Element
; - Rest-Liste
(define-record cons-list
  cons
  cons?
  (first number)
  (rest list-of-numbers))

(define list-of-numbers
  (signature (mixed empty-list
                    cons-list)))


(define list1 (cons 7 empty)) ; 1elementige Liste: 7
(define list2 (cons 7 (cons 5 empty))) ; 2elementige Liste: 7 5
(define list3 (cons 2 (cons 7 (cons 5 empty)))) ; 3elementige Liste: 2 7 5
(define list4 (cons 3 list3)) ; 4elementige Liste: 3 2 7 5
(define list5 (cons 6 list4))

; Summe einer Liste von Zahlen berechnen
(: list-sum (list-of-numbers -> number))

(check-expect (list-sum list3) 14)

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0)
      ((cons? list)
       (+ (first list)
          (list-sum (rest list)))))))

; Neutrales Element der Addition
; für alle x: x + n = n + x = x

; Neutrales Element der Multiplikation
; für alle x: x * n = n * x = x

; Produkt einer Liste von Zahlen berechnen
(: list-product (list-of-numbers -> number))

(check-expect (list-product list3) 70)

(define list-product
  (lambda (list)
    (cond
      ((empty? list) 1)
      ((cons? list)
       (* (first list)
          (list-product (rest list)))))))

; Alle positiven Zahlen aus einer Liste extrahieren
(: extract-positives (list-of-numbers -> list-of-numbers))

(check-expect (extract-positives (cons 1 (cons -5 (cons 0 (cons 3 (cons -2 empty))))))
              (cons 1 (cons 3 empty)))

(define extract-positives
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (positive? (first list))
           (cons (first list)
                 (extract-positives (rest list)))
           (extract-positives (rest list)))))))

(check-expect (extract even? list5)
              (cons 6 (cons 2 empty)))
(check-expect (extract positive? (cons 1 (cons -5 (cons 0 (cons 3 (cons -2 empty))))))
              (cons 1 (cons 3 empty)))

(define extract
  (lambda (p? list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (p? (first list))
           (cons (first list)
                 (extract p? (rest list)))
           (extract p? (rest list)))))))