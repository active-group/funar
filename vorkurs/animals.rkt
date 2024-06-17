#lang deinprogramm/sdp/beginner

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

(check-expect (not (dillo-alive? (run-over-dillo dillo1)))
              #t)

; QuickCheck
(check-property
 (for-all ((d dillo))
   (not (dillo-alive? (run-over-dillo d)))))

  
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
    (make-dillo #f ; (and (dillo-alive? dillo) (>= (dillo-weight dillo) 20))
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
  parrot?
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


; Tier überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal dillo1)
              (run-over-dillo dillo1))
(check-expect (run-over-animal parrot1)
              (run-over-parrot parrot1))

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal)
       (define dillo animal)
       (run-over-dillo dillo))
      ((parrot? animal) (run-over-parrot animal)))))

; Eine Liste ist eins der folgenden:
; - die leere Liste -ODER-
; - eine Cons-Liste bestehend aus erstem Element und Rest-Liste
;                                                         ^^^^^ Selbstbezug

; die leere Liste ... gibt nur eine ...
; brauchen: Signature, den Wert, Prädikat
;(define-singleton empty-list empty empty?)
(define-record empty-list
  make-empty
  empty?)
(define empty (make-empty))

; Eine Cons-Liste besteht aus:
; - erstes Element -UND-
; - Rest-Liste
(define-record (cons-list-of element) ; macht intern lambda
  cons
  cons?
  (first element)
  (rest (list-of element)))

(define list-of
  (lambda (element)
    (signature (mixed empty-list
                      (cons-list-of element)))))

; 1elementige Liste: 2
(define list1 (cons 2 empty))

; 2elementige Liste: 2 5
(define list2         (cons 2 (cons 5 empty)))

; 3elementige Liste: 8 2 5
;(define list3 (cons 8 (cons 2 (cons 5 empty))))
(define list3 (cons 8 list2))

; 4elementige Liste: 3 8 2 5
(define list4 (cons 3 list3))

(define list-of-numbers (signature (list-of number)))

; Liste aufsummieren
(: list-sum (list-of-numbers -> number))

(check-expect (list-sum list4)
              18)

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0) ; neutrales Element von +
      ((cons? list)
       (+ (first list)
          (list-sum (rest list)))))))

; Liste aufmultiplizieren
(: list-product (list-of-numbers -> number))

(check-expect (list-product list4)
              240)

; Schablone
#;(define list-product
  (lambda (list)
    (cond
      ((empty? list) ...)
      ((cons? list)
       ...
       (first list)
       (list-product (rest list))
       ...))))


(define list-product
  (lambda (list)
    (cond
      ((empty? list) 1) ; neutrales Element von *
      ((cons? list)
       (* (first list)
          (list-product (rest list)))))))


; aus einer Liste die geraden Zahlen extrahieren
(: extract-evens (list-of-numbers -> list-of-numbers))

(check-expect (extract-evens list4)
              (cons 8 (cons 2 empty)))

(define extract-evens
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (even? (first list))
           (cons (first list)
                 (extract-evens (rest list)))
           (extract-evens (rest list)))))))

; aus einer Liste der ungeraden Zahlen extrahieren
(: extract-odds (list-of-numbers -> list-of-numbers))

(check-expect (extract-odds list4)
              (cons 3 (cons 5 empty)))

(define extract-odds
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (odd? (first list))
           (cons (first list)
                 (extract-odds (rest list)))
           (extract-odds (rest list)))))))

; Funktion höherer Ordnung
; %element: Signaturvariable
(: extract ((%element -> boolean) (list-of %element) -> (list-of %element)))

(check-expect (extract even? list4)
              (cons 8 (cons 2 empty)))
(check-expect (extract odd? list4)
              (cons 3 (cons 5 empty)))
; Abstraktion:
; - kopieren
; - Definition: umbenennen (auch rekursive Aufrufe!)
; - Unterschiede durch abstrakte Namen ersetzen
; - lambda / Parameter: auch in rekursive Aufruf
(define extract
  (lambda (p? list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (p? (first list))
           (cons (first list)
                 (extract p? (rest list)))
           (extract p? (rest list)))))))

(define dillos (cons dillo1 (cons dillo2 empty)))

(check-expect (extract dillo-alive? dillos)
              (cons dillo1 empty))