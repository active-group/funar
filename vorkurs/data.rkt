#lang deinprogramm/sdp/beginner

; Datenanalyse

; Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; -> Fallunterscheidung/Summe, hier: Aufzählung

; "dog", "cat", "snake"

; pro Datensorte eine Signatur
(define pet
  (signature (enum "dog" "cat" "snake")))

; Haustier niedlich?

(: cute? (pet -> boolean)) ; ? wg. boolean

(check-expect (cute? "dog")
              #t)
(check-expect (cute? "cat")
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
    ; Verzweigung (<- Fallunterscheidung)
    (cond
      ; 1 Zweig pro Fall
      ; (<Bedingung> <Ergebnis>)
      ((equal? pet "dog") ...)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))

(define cute?
  (lambda (pet)
    ; Verzweigung (<- Fallunterscheidung)
    (cond
      ; 1 Zweig pro Fall
      ; (<Bedingung> <Ergebnis>)
      ((equal? pet "dog") #t)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f))))

;(cute? "gerbil")

; Uhrzeit besteht aus / hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten/Produkt

; Eine Stunde ist eine ganze Zahl zwischen 0 und 23
(define hour (signature (integer-from-to 0 23)))
; Eine Minute ist eine ganze Zahl zwischen 0 und 59
(define minute (signature (integer-from-to 0 59)))

; natural: natürliche Zahlen, 0, 1, 2, 3, 4, ...

(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour hour) ; Selektor, Getter-Funktionen
  (time-minute minute))

(: make-time (hour minute -> time))
(: time-hour (time -> hour))
(: time-minute (time -> minute))

; 11 Uhr 29 Minuten
(define time1 (make-time 11 29))
; 14:07
(define time2 (make-time 14 07))

; Minuten seit Mitternacht
(: msm (time -> natural))

(check-expect (msm time1)
              689)
(check-expect (msm time2)
              847)

; Schablone
#;(define msm
  (lambda (time)
    ... (time-hour time) ...
    ... (time-minute time) ...))

(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

; Aus Minuten seit Mitternacht die Uhrzeit berechnen

(: msm->time (natural -> time))

(check-expect (msm->time 689)
              time1)

; property-based testing / QuickCheck
(check-property
 (for-all ((t time))
   (equal? (msm->time (msm t))
           t)))

(check-property
 (for-all ((minutes natural))
   (equal? (msm (msm->time minutes))
           minutes)))

; Schablone
#;(define msm->time
  (lambda (msm)
    (make-time ... ...)))

(define msm->time
  (lambda (msm)
    (make-time (quotient msm 60)
               (remainder msm 60))))


; Tier auf dem texanischen Highway ist eins der folgenden:
; - Gürteltier -ODER-
; - Papagei
; Fallunterscheidung: jeder Fall hat eigene Datendefinition
; -> gemischten Daten
(define animal
  (signature (mixed dillo parrot)))

; "Repräsention des Zustands des Gürteltiers zu einem bestimmten Zeitpunkt."

; Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot?  -UND-
; - Gewicht
(define-record dillo
  make-dillo
  dillo? ; Prädikat
  (dillo-alive? boolean)
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

(check-property
 (for-all ((d dillo))
   (equal? (dillo-alive? (run-over-dillo d))
           #f)))

(check-property
 (for-all ((d dillo))
   (equal? (run-over-dillo d)
           (make-dillo #f (dillo-weight d)))))
           

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f
                (dillo-weight dillo))))

; Gürteltier füttern, um bestimmte Menge Essen
; nur lebendige Gürteltiere fressen

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


(define feed-dillo
  (lambda (dillo amount)
    (define alive? (dillo-alive? dillo))
    (define weight (dillo-weight dillo))
    (make-dillo alive?
                (if alive?
                    (+ weight amount)
                    weight)
                #;(cond
                  ((dillo-alive? dillo)
                   (+ (dillo-weight dillo) amount))
                  (else ; (not (dillo-alive? dillo))
                   (dillo-weight dillo))))))

; Ein Papagei hat folgende Eigenschaften:
; - Satz -UND-
; - Gewicht
(define-record parrot
  make-parrot
  parrot?
  (parrot-sentence string)
  (parrot-weight number))

(define parrot1 (make-parrot "Welcome!" 1))
(define parrot2 (make-parrot "Auf Wiedersehen!" 2))

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
(check-expect (run-over-animal parrot1)
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

#|
Open/Closed Principle:
- FP neue Fälle :-(
- OOP neue Fälle :-)
- FP neue Funktionen :-)
- OOP neue Funktionen :-(

- beides gut möglich? "Expression problem"
|#

; Gewicht eines Tiers
(: animal-weight (animal -> number))

(define animal-weight
  (lambda (animal)
    (cond
      ((dillo? animal) (dillo-weight animal))
      ((parrot? animal) (parrot-weight animal)))))


; Liste ist eins der folgenden:
; - die leere Liste   -ODER-
; - eine Cons-Liste
;   bestehend aus erstem Element und Rest-Liste
;                                         ^^^^^ Selbstbezug
(define list-of
  (lambda (element)
    (signature (mixed empty-list
                      (cons-list-of element)))))

; Singleton:
; die leere Liste
(define-singleton empty-list ; Signatur
  empty ; Singleton
  empty?) ; Prädikat

;(: empty empty-list)

; Cons-Liste besteht aus:
; - erstes Element -UND-
; - Rest-Liste
(define-record (cons-list-of element)
  cons
  cons?
  (first element)
  (rest (list-of element)))

; 1elementige Liste: 5
(define list1 (cons 5 empty))
; 2elementige Liste: 2 5
(define list2 (cons 2 (cons 5 empty)))
; 3elementige Liste:   7 2 5
(define list3 (cons 7 (cons 2 (cons 5 empty))))
; 4elementige Liste: 6 7 2 5
(define list4 (cons 6 list3))

(define list-of-numbers (signature (list-of number)))

; Liste aufsummieren
(: list-sum ((list-of number) -> number))

(check-expect (list-sum list4)
              20)

; Schablone
#;(define list-sum
  (lambda (list)
    (cond
      ((empty? list) ...)
      ((cons? list)
       ... (first list) ...
       ... (list-sum (rest list)) ...)))) ; (rest list) steht nirgendwo sonst

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0) ; neutrale Element der Addition
      ((cons? list)
       (+ (first list)
          (list-sum (rest list)))))))

; Liste aufmultiplizieren
(: list-product (list-of-numbers -> number))

(check-expect (list-product list4)
              420)

(define list-product
  (lambda (list)
    (cond
      ((empty? list) 1)
      ((cons? list)
       (* (first list)
          (list-product (rest list)))))))

; Aus einer Liste die ungeraden Zahlen extrahieren
(: extract-odds (list-of-numbers -> list-of-numbers))

(check-expect (extract-odds list4)
              (cons 7 (cons 5 empty)))

(define extract-odds
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (odd? (first list))
           (cons (first list)
                 (extract-odds (rest list)))
           (extract-odds (rest list)))))))
              
; Abstrahieren:
; - letztes Mal kopieren
; - umbenennen (an rekursive Aufrufe denken!)
; - Unterschiede durch abstrakte Namen ersetzen
; - Namen in lambda aufnehmen (an rekursive Aufrufe denken!)

; (: even? (number -> boolean))

; Funktion höherer Ordnung: >1 Pfeil
;(: extract ((number -> boolean) list-of-numbers -> list-of-numbers))
; %element: Signaturvariable
(: extract ((%element -> boolean) (list-of %element) -> (list-of %element)))

(define extract
  (lambda (p? list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (p? (first list))
           (cons (first list)
                 (extract p? (rest list)))
           (extract p? (rest list)))))))

(define dillos
  (cons dillo1 (cons dillo2 empty)))

(define highway
  (cons dillo1 (cons dillo2 (cons parrot1 (cons parrot2 empty)))))

; Alle Tiere überfahren
(: run-over-animals ((list-of animal) -> (list-of animal)))

(check-expect (run-over-animals highway)
              (cons (run-over-animal dillo1)
                    (cons (run-over-animal dillo2)
                          (cons (run-over-animal parrot1)
                                (cons (run-over-animal parrot2)
                                      empty)))))

(define run-over-animals
  (lambda (list)
    (cond
      ((empty? list) ...)
      ((cons? list)
       (first list)
       (run-over-animals (rest list))
       ...))))


                                     