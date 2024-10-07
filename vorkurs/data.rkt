#lang deinprogramm/sdp/beginner

; Datenanalyse

; Uhrzeit besteht aus/hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour   (integer-from-to 0 23)) ; Selektor ("Getter-Funktion")
  (time-minute (integer-from-to 0 59)))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; 10 Uhr 54 Minuten
(define time1 (make-time 10 54))
(define time2 (make-time 14 32))

; Minuten seit Mitternacht
(: minutes-since-midnight (time -> natural))

(check-expect (minutes-since-midnight time1)
              654)
(check-expect (minutes-since-midnight time2)
              872)

; Gerüst
#;(define minutes-since-midnight
  (lambda (time)
    ...))

; Schablone, entsteht aus der Signatur
#;(define minutes-since-midnight
  (lambda (time)
    ; zusammengesetzte Daten als Input
    ... (time-hour time) ...
    ... (time-minute time) ...))

(define minutes-since-midnight
  (lambda (time)
    ; zusammengesetzte Daten als Input
    (+ (* 60 (time-hour time))
       (time-minute time))))

; aus Minuten-seit-Mitternacht die Uhrzeit berechnen
(: msm->time (natural -> time))

(check-expect (msm->time 654)
              (make-time 10 54))

(define msm->time
  (lambda (minutes)
    (define hour (quotient minutes 60))
    (define minute (remainder minutes 60))
    (make-time hour minute)))

(check-property
 (for-all ((t time))
   (equal? t
           (msm->time (minutes-since-midnight t)))))


; Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; Fallunterscheidung
; hier: Aufzählung
(define pet
  (signature (enum "dog"
                   "cat"
                   "snake")))

; Ist Haustier niedlich?
(: cute? (pet -> boolean))

(check-expect (cute? "dog") #t)
(check-expect (cute? "cat") #t)
(check-expect (cute? "snake") #f)

; Schablone
#;(define cute?
  (lambda (pet)
    ; Fallunterschiedung -> Verzweigung
    ; für jeden Fall einen Zweig
    ; werden sequentiell abgearbeitet
    (cond
      ; Zweig: (<Bedingung> <Ergebnis>)
      ((equal? pet "dog") ...)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))

(define cute?
  (lambda (pet)
    ; Fallunterschiedung -> Verzweigung
    ; für jeden Fall einen Zweig
    ; werden sequentiell abgearbeitet
    (cond
      ; Zweig: (<Bedingung> <Ergebnis>)
      ((equal? pet "dog") #t)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f))))

; Tier auf dem texanischen Highway:
; - Gürteltier -ODER-
; - Papagei
; Fallunterscheidung, jeder Fall eigene Datendefinition
; -> gemischte Daten
(define animal
  (signature (mixed dillo
                    parrot)))

; Gürteltier hat folgende Eigenschaften:
; - lebendig? -UND-
; - Gewicht
; eigentlich: (Repräsentation des) Zustand des Gürteltiers zu einem bestimmten Zeitpunkt
(define-record dillo
  make-dillo
  dillo? ; Prädikat
  (dillo-alive? boolean)
  (dillo-weight number))

(: dillo? (any -> boolean))

; lebendiges Gürteltier 10kg
(define dillo1 (make-dillo #t 10))
; totes Gürteltier 8kg
(define dillo2 (make-dillo #f 8))

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))
(check-expect (run-over-dillo dillo2)
              dillo2)

(check-property
 (for-all ((d dillo))
   (not (dillo-alive? (run-over-dillo d)))))

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))
  
; Gürteltier füttern (mit einer spezifizierbaren Menge Futter)
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 5)
              (make-dillo #t 15))
(check-expect (feed-dillo dillo2 5)
              dillo2)

(define feed-dillo
  (lambda (dillo amount)
    (define alive? (dillo-alive? dillo))
    (define weight (dillo-weight dillo))
    (make-dillo alive?
                ; binäre Verzweigung
                (if alive?
                    (+ weight amount)
                    weight)
                #;(cond
                  (alive?
                   (+ weight amount))
                  (else
                   #;(not (dillo-alive? dillo))
                   weight)))))


; Papagei hat folgende Eigenschaften:
; - Satz -UND-
; - Gewicht
(define-record parrot
  make-parrot
  parrot?
  (parrot-sentence string)
  (parrot-weight number))

; 1kg-Papagei, sagt "hello!"
(define parrot1 (make-parrot "hello!" 1))
(define parrot2 (make-parrot "goodbye!" 2))

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot1)
              (make-parrot "" 1))
              
(define run-over-parrot
  (lambda (parrot)
    (make-parrot "" (parrot-weight parrot))))


; Tier überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal dillo1)
              (run-over-dillo dillo1))
(check-expect (run-over-parrot parrot1)
              (run-over-parrot parrot1))

; Schablone:
#;(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) ...)
      ((parrot? animal) ...))))

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (run-over-dillo animal))
      ((parrot? animal) (run-over-parrot animal)))))

; hier:
; gemischte Daten
; jeder Fall ist zusammengesetzte Daten

; Liste ist eins der folgenden:
; - die leere Liste -ODER-
; - eine Cons-Liste bestehend aus erstem Element und Rest-Liste
;                                                         ^^^^^ Selbstbezug

(define list-of
  (lambda (element)
    (signature (mixed empty-list
                      (cons-list-of element)))))

; Die leere Liste hat folgende Eigenschaften:
; ... ein Singleton
(define-record empty-list
  make-empty-list
  empty?)

; Convenience
(define empty (make-empty-list))

; Eine Cons-Liste besteht aus:
; - erstes Element -UND-
; - Rest-List
(define-record (cons-list-of element) ; macht Hintergrund ein lambda
  cons
  cons?
  (first element)
  (rest (list-of element)))

; 1elementige Liste: 5
(define list1 (cons 5 empty))
; 2elementige Liste: 2 5
(define list2 (cons 2 (cons 5 empty)))
; 3elementige Liste: 7 2 5
(define list3 (cons 7 (cons 2 (cons 5 empty))))
; 4elementige Liste 8 7 2 5
(define list4 (cons 8 list3))

; Elemente einer Liste addieren
(: list-sum ((list-of number) -> number))

(check-expect (list-sum list4)
              22)

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
      ((empty? list) 0) ; das neutrale Element von +
      ((cons? list)
       (+ (first list)
          (list-sum (rest list)))))))

(define list-of-numbers (signature (list-of number)))

; Produkt der Elemente einer Liste berechnen
(: list-product (list-of-numbers -> number))

(check-expect (list-product list4)
              560)

(define list-product
  (lambda (list)
    (cond
      ((empty? list) 1) ; das neutrale Element von *
      ((cons? list)
       (* (first list)
          (list-product (rest list)))))))

; für alle x: x * 1 = 1 * x = x
;             x + 0 = 0 + x = x

; aus einer Liste die ungeraden Zahlen herausextrahieren
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
    
; Abstraktion über 2 Funktionsdefinitionen
; - kopieren
; - umbenennen - an die rekursiven Aufrufe denken
; - Unterschiede durch abstrakte Namen ersetzen
; - Namen in lambda aufnehmen - an die rekursiven Aufrufe denken

; oft eingebaut als filter


; parametrische Polymorphie
; %: Signaturvariable, "magisch", bei jedem Aufruf kann das was anderes sein
(: extract ((%element -> boolean) (list-of %element) -> (list-of %element)))

(check-expect (extract even? list4)
              (cons 8 (cons 2 empty)))
(check-expect (extract odd? list4)
              (cons 7 (cons 5 empty)))
(define extract
  (lambda (p? list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (p? (first list))
           (cons (first list) (extract p? (rest list)))
           (extract p? (rest list)))))))

(define dillos (cons dillo1 (cons dillo2 empty)))

; (extract dillo-alive? dillos)

; Alle Gürteltiere überfahren
(: run-over-dillos ((list-of dillo) -> (list-of dillo)))

(check-expect (run-over-dillos dillos)
              (cons (run-over-dillo dillo1)
                    (cons (run-over-dillo dillo2)
                          empty)))

(define run-over-dillos
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons
        (run-over-dillo (first list))
        (run-over-dillos (rest list)))))))

; Liste von Zahlen inkrementieren
(: inc-list ((list-of number) -> (list-of number)))

(check-expect (inc-list list4)
              (cons 9 (cons 8 (cons 3 (cons 6 empty)))))

(define inc-list
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons
        (inc (first list))
        (inc-list (rest list)))))))

(define inc (lambda (n) (+ 1 n)))

(: map-list ((%a -> %b) (list-of %a)  -> (list-of %b)))

(define map-list
  (lambda (f list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons
        (f (first list))
        (map-list f (rest list)))))))

(define list-fold
  (lambda (neutral-element operation list)
    (cond
      ((empty? list) neutral-element)
      ((cons? list)
       (operation (first list)
          (list-fold neutral-element operation (rest list)))))))
