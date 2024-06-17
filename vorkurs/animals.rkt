#lang deinprogramm/sdp

; Datenanalyse

; F#: type pet = dog | cat | snake

; Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange -ODER-
; - Schwein
; Fallunterscheidung, hier speziell: Aufzählung
(define pet
  (signature (enum "dog" "cat" "snake" "pig")))

; Ist ein Haustier niedlich?
(: cute? (pet -> boolean))

(check-expect (cute? "dog") #t)
(check-expect (cute? "cat") #t)
(check-expect (cute? "snake") #f)
(check-expect (cute? "pig") #t)

; Gerüst
#;(define cute?
  (lambda (pet)
    ...))

; Schablone, ausschließlich aus der Signatur
#;(define cute?
  (lambda (pet)
    (cond ; Verzweigung, 1 Zweig pro Fall
      ((equal? pet "dog") ...) ; (<Bedingung> <Ergebnis>)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))

(define cute?
  (lambda (pet)
    (cond ; Verzweigung, 1 Zweig pro Fall
      ((equal? pet "dog") #t) ; (<Bedingung> <Ergebnis>)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f)
      ((equal? pet "pig") #t))))

; Tiere auf dem texanischen Highway

; Repräsentation des Zustands des Gürteltiers zu einem bestimmten Zeitpunkt

; Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot? -UND-
; - Gewicht
; zusammengesetzte Daten
(define-record dillo ; Signatur
  make-dillo ; Konstruktor
  dillo? ; Prädikat
  (dillo-alive? boolean) ; Selektor
  (dillo-weight number))

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

  
; Schablone - zusammengesetzte Daten als Eingabe

#;(define run-over-dillo
  (lambda (dillo)
    ... (dillo-alive? dillo) ...
    ... (dillo-weight dillo) ...))

; Schablone - zusammengesetzte Daten als Ausgabe

#;(define run-over-dillo
  (lambda (dillo)
    (make-dillo ... ...)))

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f
                (dillo-weight dillo))))

(define dillo3 (run-over-dillo dillo1))

; Gürteltier füttern - wählbare Futtermenge

(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 5)
              (make-dillo #t 15))
(check-expect (feed-dillo dillo2 5)
              dillo2)

#;(define feed-dillo
  (lambda (dillo amount)
    (make-dillo (dillo-alive? dillo)
                (cond
                  ((equal? (dillo-alive? dillo) #t)
                   (+ (dillo-weight dillo) amount))
                  ((equal? (dillo-alive? dillo) #f)
                   (dillo-weight dillo))))))

#;(define feed-dillo
  (lambda (dillo amount)
    (make-dillo (dillo-alive? dillo)
                (if (dillo-alive? dillo)
                    (+ (dillo-weight dillo) amount)
                    (dillo-weight dillo))
                #;(cond ; binäre Verzweigung
                  ((dillo-alive? dillo)
                   (+ (dillo-weight dillo) amount))
                  (else
                   (dillo-weight dillo))))))

(define feed-dillo
  (lambda (dillo amount)
    (if (dillo-alive? dillo)
        (make-dillo #t (+ (dillo-weight dillo) amount))
        dillo
        #;(make-dillo (dillo-alive? dillo) (dillo-weight dillo)))))

; Papagei hat folgende Eigenschaften:
; - Satz -UND-
; - Gewicht
(define-record parrot
  make-parrot
  (parrot-sentence string)
  (parrot-weight number))

; Begrüßungspapagei
(define parrot1 (make-parrot "Hello!" 1))
(define parrot2 (make-parrot "Goodbye!" 2))

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot1)
              (make-parrot "" 1))

(define run-over-parrot
  (lambda (parrot)
    (make-parrot ""
                 (parrot-weight parrot))))

; Ein Tier ist eins der folgenden:
; - Gürteltier -ODER-
; - Papagei
; Fallunterscheidung
; hier: gemischte Daten
(define animal (signature (mixed dillo parrot)))

#|
; Tier überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal dillo1)
              (run-over-dillo dillo1))
(check-expect (run-over-animal parrot1)
              (run-over-parrot parrot1))

(define run-over-animal
  (lambda (animal)
    (cond
      (... ...)
      (... ...))))

|#