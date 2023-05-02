#lang deinprogramm/sdp/beginner
; Datenanalyse

; Datendefinition

; Ein Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; -> Fallunterscheidung
; hier: Aufzählung ausreichend, "dog", "cat", "snake"

; Datendefinition -> Code, Signatur
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

; Schablone
; pet: Fallunterscheidung
#;(define cute?
  (lambda (pet)
    ; Verzweigung, 1 Zweig pro Fall
    ; jeder Zweig: (<Bedingung> <Ergebnis>)
    (cond
      ((string=? pet "dog") ...)
      ((string=? pet "cat") ...)
      ((string=? pet "snake") ...))))

#;(define cute?
  (lambda (pet)
    ; Verzweigung, 1 Zweig pro Fall
    ; jeder Zweig: (<Bedingung> <Ergebnis>)
    (cond
      ((string=? pet "dog") #t)
      ((string=? pet "cat") #t)
      ((string=? pet "snake") #f))))

(define cute?
  (lambda (pet)
    (match pet
      ("dog" #t)
      ("cat" #t)
      ("snake" #f))))

; Eine Uhrzeit besteht aus / hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten
; Record-Definition
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour ; Selektor
   natural) ; Signatur, natürliche Zahlen 
  (time-minute natural))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; 12 Uhr 24
(define time1 (make-time 12 24))
; 14:12 Uhr
(define time2 (make-time 14 12))

; Minuten seit Mitternacht berechnen
(: minutes-since-midnight (time -> natural))

(check-expect (minutes-since-midnight time1)
              744)
(check-expect (minutes-since-midnight time2)
              852)

; Schablone
#;(define minutes-since-midnight
  (lambda (time)
    ...
    (time-hour time)
    (time-minute time)
    ...))

(define minutes-since-midnight
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

(: create-time-from-minutes (natural -> time))

(check-expect (create-time-from-minutes 744)
              time1)

(define create-time-from-minutes
  (lambda (min)
    (make-time (quotient min 60) (remainder min 60))))

; Ein Tier auf dem texanischen Highway ist eins der folgenden:
; - Gürteltier -ODER-
; - Papagei
; gemischte Daten
(define animal
  (signature (mixed dillo
                    parrot)))

; Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot -UND-
; - Gewicht
; zusammengesetzte Daten
(define-record dillo
  make-dillo
  dillo? ; Prädikat
  (dillo-alive? boolean)
  (dillo-weight number))

(: dillo? (any -> boolean))

(: make-dillo (boolean number -> dillo))
(: dillo-alive? (dillo -> boolean))
(: dillo-weight (dillo -> number))

; Gürteltier, lebendig, 10kg
(define dillo1 (make-dillo #t 10))
; totes Gürteltier, 8kg
(define dillo2 (make-dillo #f 8))

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))
(check-expect (run-over-dillo dillo2)
              dillo2)

; Schablone
#;(define run-over-dillo
  (lambda (dillo)
    (make-dillo ... ...)
    ...
    (dillo-alive? dillo)
    (dillo-weight dillo)
    ...))

#;(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))

(define run-over-dillo
  (lambda (dillo)
    (match dillo
      ((make-dillo #t w) (make-dillo #f w))
      ((make-dillo #f w) dillo))))


(define feed-dillo
  (lambda (dillo w)
    (if (dillo-alive? dillo)
        (make-dillo #t (+ w (dillo-weight dillo)))
        dillo)))

; Papagei hat folgende Eigenschaften:
; - Satz -UND-
; - Gewicht
(define-record parrot
  make-parrot
  parrot?
  (parrot-sentence string)
  (parrot-weight number))

; Begrüßungs-Papagei
(define parrot1 (make-parrot "Welcome!" 1))
; Rausschmeißer
(define parrot2 (make-parrot "Goodbye!" 2))

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot1)
              (make-parrot "" 1))
(check-expect (run-over-parrot parrot2)
              (make-parrot "" 2))

(define run-over-parrot
  (lambda (parrot)
    (make-parrot "" (parrot-weight parrot))))

; Tier überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal parrot1)
              (run-over-parrot parrot1))
(check-expect (run-over-animal dillo1)
              (run-over-dillo dillo1))

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (run-over-dillo animal))
      ((parrot? animal) (run-over-parrot animal)))))

; Ein Duschprodukt:
; - Seife -ODER-
; - Shampoo -ODER-
; - Mixtur aus zwei Duschprodukten
;                   ^^^^^^^^^^^^ Selbstbezug

; gelöscht:
; - Duschgel (50% Seife, 50% Shampoo)

; Bei Seife interessiert der pH-Wert.
; Beim Shampoo der Haartyp (normal, fettig, Schuppen)

; Seife hat folgende Bestandteile:
; - pH-Wert
(define-record soap
  make-soap
  soap?
  (soap-ph number))

(define hair-type
  (signature (enum "normal" "oily" "dandruff")))

(define-record shampoo
  make-shampoo
  shampoo?
  (shampoo-hair-type hair-type))

; Eine Mixtur besteht aus:
; - einem Duschprodukt
; - noch einem Duschprodukt

(define-record mixture
  make-mixture
  mixture?
  (mixture-product1 shower-product)
  (mixture-product2 shower-product))

(define shower-product
  (signature (mixed shampoo soap mixture)))

(define soap1 (make-soap 7))
(define soap2 (make-soap 9))
(define shampoo1 (make-shampoo "oily"))
(define shampoo2 (make-shampoo "normal"))

(define mix1 (make-mixture soap1 shampoo1))
(define mix2 (make-mixture mix1 soap2))


; Seifenanteil eines Duschprodukts berechnen
(: shower-product-soap (shower-product -> number))

(check-expect (shower-product-soap soap1) 1)
(check-expect (shower-product-soap shampoo1) 0)
(check-expect (shower-product-soap mix1) 0.5)
(check-expect (shower-product-soap mix2) 0.75)

(define shower-product-soap
  (lambda (product)
    (cond
      ((soap? product) 1)
      ((shampoo? product) 0)
      ((mixture? product)
       (/
        (+
         (shower-product-soap (mixture-product1 product))
         (shower-product-soap (mixture-product2 product)))
        2)))))

; Eine Liste ist eins der folgenden:
; - die leere Liste -ODER-
; - eine Cons-Liste, bestehend aus erstem Element und Rest-Liste
;                                                          ^^^^^^
(define list-of
  (lambda (element)
    (signature (mixed empty-list
                      (cons-list-of element)))))

(define-record empty-list
  make-empty
  empty?)
(define empty (make-empty))

; Eine Cons-Liste besteht aus:
; - erstes Element
; - Rest-Liste
(define-record (cons-list-of element) ; macht hinter den Kulissen ein lambda
  cons
  cons?
  (first element)
  (rest (list-of element)))

(define list-of-numbers (signature (list-of number)))

; 1elementige Liste: 5
(define list1 (cons 5 empty))
; 2elementige Liste: 2 5
(define list2 (cons 2 (cons 5 empty)))
; 3elementige Liste: 7 2 5
(define list3 (cons 7 (cons 2 (cons 5 empty))))
; 4elementige Liste: 4 7 2 5
(define list4 (cons 4 list3))

; Summe aller Listenelemente
(: list-sum (list-of-numbers -> number))

(check-expect (list-sum list4) 18)

; Schablone:
#;(define list-sum
  (lambda (list)
    (cond
      ((empty? list) ...)
      ((cons? list)
       ...
       (first list)
       (list-sum (rest list))
       ...))))

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0) ; neutrales Element bezüglich +
      ((cons? list)
       (+ (first list)
          (list-sum (rest list)))))))

; Produkt aller Listenelemente
(: list-product (list-of-numbers -> number))

(check-expect (list-product list4)
              280)

(define list-product
  (lambda (list)
    (cond
      ((empty? list) 1) ; neutrales Element bezüglich *
      ((cons? list)
       (*
        (first list)
        (list-product (rest list)))))))

(define identity (lambda (x) x))


; Aus einer Liste von Zahlen die geraden Elemente
(: extract-odds (list-of-numbers -> list-of-numbers))

(check-expect (extract-odds list4)
              (cons 7 (cons 5 empty)))

(define extract-odds
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (odd? (first list))
           (cons (first list) (extract-odds (rest list)))
           (extract-odds (rest list)))))))

(define highway (cons dillo1 (cons dillo2 empty)))

; Abstraktion:
; - kopieren
; - umbenennen (rekursive Aufrufe nicht vergessen!)
; - den Unterschieden Namen geben
; - die Namen in ein lambda aufnehmen (rekursive Aufrufe ...)

; Signaturvariable: %element -> parametrische Polymorphie
; Higher-Order-Funktion ("Funktion mit mehr als einem Pfeil")
(: extract-list ((%element -> boolean) (list-of %element) -> (list-of %element)))
; heißt i.d.R. filter

(define extract-list
  (lambda (p? list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (p? (first list))
           (cons (first list) (extract-list p? (rest list)))
           (extract-list p? (rest list)))))))

(extract-list dillo-alive? highway)

; Alle Tiere auf dem Highway überfahren
(: run-over-animals ((list-of animal) -> (list-of animal)))

(check-expect (run-over-animals highway)
              (cons (run-over-animal dillo1)
                    (cons (run-over-animal dillo2)
                          empty)))

(define run-over-animals
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons
        (run-over-animal (first list))
        (run-over-animals (rest list)))))))

; Alle Zahlen einer Liste inkrementieren
(: list-inc ((list-of number) -> (list-of number)))

(check-expect (list-inc list4)
              (cons 5 (cons 8 (cons 3 (cons 6 empty)))))

(define list-inc
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons
        (+ 1 (first list))
        (list-inc (rest list)))))))