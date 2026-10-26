#lang deinprogramm/sdp/beginner
; Datenanalyse

; Haustier ist eins der folgenden:
; - Hund      ODER
; - Katze     ODER
; - Schlange
; Fallunterscheidung, hier: Aufzählung

(define pet
  (signature (enum "dog"
                   "cat"
                   "snake")))

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
    ; Verzweigung:
    ; 1 Zweig pro Fall (<Bedingung> <Ergebnis>)
    (cond
      ((equal? pet "dog") ...)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))

(define cute?
  (lambda (pet)
    ; Verzweigung:
    ; 1 Zweig pro Fall (<Bedingung> <Ergebnis>)
    (cond
      ((equal? pet "dog") #t)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f))))

; Uhrzeit besteht aus / hat folgende Eigenschaften:
; - Stunde  UND
; - Minute
; zusammengesetzte Daten / Produkt
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour natural) ; Selektor, "Getter-Funktion"
  (time-minute natural))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; 11 Uhr 4 Minuten
(define time1 (make-time 11 04))
; 15:13 Uhr
(define time2 (make-time 15 13))

; Minuten seit Mitternacht
(: minutes-since-midnight (time -> natural))

(check-expect (minutes-since-midnight time1)
              664)
(check-expect (minutes-since-midnight time2)
              913)

; Schablone
#;(define minutes-since-midnight
  (lambda (time)
    ... (time-hour time) ...
    ... (time-minute time) ...))


(define minutes-since-midnight
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

; Übungsaufgabe:
; Aus Minuten-seit-Mitternacht die Uhrzeit berechnen

(: msm-to-time (natural -> time))

(check-expect (msm-to-time 664) time1)

; Schablone

#;(define msm-to-time
  (lambda (minutes)
    (make-time ... ...)))

(define msm-to-time
  (lambda (minutes)
    (make-time 
     (quotient minutes 60)
     (remainder minutes 60))))

; Tiere auf dem texanischen Highway

; Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot  -UND-
; - Gewicht
; Zustand des Gürteltiers zu einem bestimmten Zeitpunkt
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

; totes Gürteltier, 8k
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


#| OO
class Dillo {
  bool isAlive;

  void runOver() {
     this.isAlive = false;
  }

|#

; Gürteltier füttern, Futtermenge variabel, totes Gürteltier nimmt nicht zu

; Gürteltier futtern, totes Gürteltier nimmt nicht zu
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 3)
              (make-dillo #t 13))
(check-expect (feed-dillo dillo2 5)
              (make-dillo #f 8))

#;(define feed-dillo
  (lambda (dillo food-weight)
    (cond
      ((equal? (dillo-alive? dillo) #t)
       (make-dillo #t
                   (+ (dillo-weight dillo)
                      food-weight)))
      ((equal? (dillo-alive? dillo) #f)
       dillo))))

#;(define feed-dillo
  (lambda (dillo food-weight)
    (cond
      ((dillo-alive? dillo)
       (make-dillo #t
                   (+ (dillo-weight dillo)
                      food-weight)))
      (else   ; (not (dillo-alive? dillo))
       dillo))))

#;(define feed-dillo
  (lambda (dillo food-weight)
    (if (dillo-alive? dillo)
        (make-dillo #t
                    (+ (dillo-weight dillo)
                       food-weight))
        dillo)))

(define feed-dillo
  (lambda (dillo food-weight)
    (define alive? (dillo-alive? dillo))
    (define weight (dillo-weight dillo))
    (make-dillo alive?
                (if alive?
                    (+ weight food-weight)
                    weight))))

; Papagei hat folgende Eigenschaften:
; - Satz     UND
; - Gewicht
(define-record parrot
  make-parrot
  parrot?
  (parrot-sentence string)
  (parrot-weight number))

; Begrüßungs-Papagei
(define parrot1 (make-parrot "Welcome!" 1))
; Verabschiedungs-Papagei
(define parrot2 (make-parrot "Au revoir!" 2))

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot1)
              (make-parrot "" 1))

(define run-over-parrot
  (lambda (parrot)
    (make-parrot "" (parrot-weight parrot))))

; Tier (auf dem texanischen Highway) ist eins der folgenden:
; - Gürteltier ODER
; - Papagei
; Fallunterscheidung, hier gemischte Daten, Summe
(define animal
  (signature (mixed dillo parrot)))

; Tier überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal dillo1)
              (make-dillo #f 10))
(check-expect (run-over-animal parrot1)
              (make-parrot "" 1))

; Schablone
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

; Eine Liste ist eins der folgenden:
; - die leere Liste   ODER
; - eine Cons-Liste aus erstem Element und Rest-Liste
;                                               ^^^^^ Selbstbezug

#;(define list-of-numbers
  (signature (mixed empty-list cons-list)))

(define list-of
  (lambda (element) ; Signatur der Elemente
    (signature (mixed empty-list
                      (cons-list-of element)))))

(define-singleton empty-list ; Signatur
  empty ; die leere Liste (Singleton)
  empty?) ; Prädikat

; Eine Cons-Liste besteht aus:
; - erstes Element UND
; - Rest-Liste
(define-record (cons-list-of element) ; macht im Hintergrund ein (lambda (element) ...)
  cons
  cons?
  (first element)
  (rest (list-of element)))

; 1elementige Liste: 5
(define list1 (cons 5 empty))
; 2elementige Liste: 2 5
(define list2 (cons 2 (cons 5 empty)))
; 3elementige Liste: 2 5 8
(define list3         (cons 2 (cons 5 (cons 8 empty))))
; 4elementige Liste: 7 2 5 8
;(define list4 (cons 7 (cons 2 (cons 5 (cons 8 empty)))))
(define list4 (cons 7 list3))

(define list-of-numbers (signature (list-of number)))

; Liste aufsummieren
(: list-sum ((list-of number) -> number))

(check-expect (list-sum list4)
              22)


; Schablone
#;(define list-sum
  (lambda (list)
    (cond
      ((empty? list) ...)
      ((cons? list)
       ...
       (first list)
       (list-sum (rest list))
       ...
       ))))

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0)
      ((cons? list)
       (+ (first list)
          (list-sum (rest list)))))))

; Liste aufmultiplizieren
(: list-product (list-of-numbers -> number))

(check-expect (list-product list4)
              560)

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

; Abstraktion:
; - 2 ähnliche/verwandte Stücken Code

; - kopieren (ein letztes Mal) & umbenennen; aufpassen bei den rekursiven Aufrufen
; - Unterschiede durch (abstrakte) Namen ersetzen
; - Namen in Lambda-Ausdruck aufnehmen; rekursive Aufrufe mitnehmen

;(: extract ((number -> boolean) list-of-numbers -> list-of-numbers))
; %element: Signaturvariable; bei jedem Aufruf andere Signatur
(: extract ((%element -> boolean) (list-of %element) -> (list-of %element)))

(check-expect (extract even? list4)
              (cons 2 (cons 8 empty)))
(check-expect (extract odd? list4)
              (cons 7 (cons 5 empty)))

(define extract
  (lambda (p? list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (p? (first list))
           (cons (first list)
                 (extract p? (rest list)))
           (extract p? (rest list)))))))

(: dillos (list-of dillo))
(define dillos (cons dillo1 (cons dillo2 empty)))

; alle Tiere überfahren
(: run-over-animals ((list-of animal) -> (list-of animal)))

(define highway (cons dillo1 (cons dillo2 (cons parrot1 (cons parrot2 empty)))))

(check-expect (run-over-animals highway)
              (cons (run-over-animal dillo1)
                    (cons (run-over-animal dillo2)
                          (cons (run-over-animal parrot1)
                                (cons (run-over-animal parrot2)
                                      empty)))))

(define run-over-animals
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons
        (run-over-animal (first list))
        (run-over-animals (rest list)))))))

; alle Zahlen einer Liste inkrementieren
(: inc-list ((list-of number) -> (list-of number)))

(check-expect (inc-list list4)
              (cons 8 (cons 3 (cons 6 (cons 9 empty)))))

(define inc-list
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons
        (inc (first list))
        (inc-list (rest list)))))))

(define inc (lambda (x) (+ 1 x)))

;(: map-list ((%element1 -> %element2) (list-of %element1) -> (list-of %element2)))

(: map-list ((%a -> %b) (list-of %a) -> (list-of %b)))

(define map-list
  (lambda (f list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons
        (f (first list))
        (map-list f (rest list)))))))

(: list-fold ((%a %b -> %b) %b (list-of %a) -> %b))



(define list-fold
  (lambda (f e list)
    (cond
      ((empty? list) e)
      ((cons? list)
       (f (first list)
          (list-fold f e (rest list)))))))
