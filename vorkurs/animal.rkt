#lang deinprogramm/sdp/beginner
; Tiere auf dem texanischen Highway

; Gürteltier hat folgende Eigenschaften:  <- zusammengesetzte Daten
; - lebendig oder tot?
; - Gewicht
(define-record dillo
  make-dillo
  dillo? ; Prädikat
  (dillo-alive? boolean)
  (dillo-weight number))

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

; Gilt nicht:
#;(check-property
 (for-all ((d dillo))
   (equal? (run-over-dillo d)
           d)))

(check-property
 (for-all ((d dillo))
   (not (dillo-alive? (run-over-dillo d)))))

#;(define run-over-dillo
    (lambda (dillo)
      (make-dillo ... ...)
      (dillo-alive? dillo)
      (dillo-weight dillo)))
      
(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))

; Gürteltier füttern
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 5)
              (make-dillo #t 15))
(check-expect (feed-dillo dillo2 5)
              dillo2)

#;(define feed-dillo
  (lambda (dillo amount)
    ; Fallunterscheidung
    ; zusammengesetzte Daten
    (define alive? (dillo-alive? dillo))
    (define weight (dillo-weight dillo))
    (make-dillo
     alive?
     (if alive?
         (+ weight amount)
         weight)
     #;(cond
       ((dillo-alive? dillo) (+ (dillo-weight dillo) amount))
       (else (dillo-weight dillo))))))

#;(define feed-dillo
  (lambda (dillo amount)
    (if (dillo-alive? dillo)
        (make-dillo #t (+ (dillo-weight dillo) amount))
        dillo)))

(define feed-dillo
  (lambda (dillo amount)
    (match dillo
      ((make-dillo #t weight) (make-dillo #t (+ weight amount)))
      ((make-dillo #f weight) dillo))))

; Papagei hat folgende Eigenschaften:
; - Satz -UND-
; - Gewicht
(define-record parrot
  make-parrot
  parrot?
  (parrot-sentence string)
  (parrot-weight number))

; Begrüßungs-Papagei
(define parrot1 (make-parrot "Hallo!" 1))
; Verabschiedungs-Papagei
(define parrot2 (make-parrot "Tschüss!" 2))

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot1)
              (make-parrot "" 1))

(define run-over-parrot
  (lambda (parrot)
    (make-parrot "" (parrot-weight parrot))))

; lexikalische Bindung:
; vom Vorkommen aus von innen nach außen suchen, nach lambda oder define

; Ein Tier ist eins der folgenden:
; - Gürteltier -ODER-
; - Papagei
; Fallunterscheidung
; hier speziell: gemischte Daten
(define animal
  (signature (mixed dillo parrot)))

; Tier überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal dillo1)
              (run-over-dillo dillo1))
(check-expect (run-over-animal parrot1)
              (run-over-parrot parrot1))

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (run-over-dillo animal))
      ((parrot? animal) (run-over-parrot animal)))))

; Eine Liste ist eins der folgenden:
; - die leere Liste
; - eine Cons-Liste, bestehend aus:
;     erstem Element und Rest-Liste
;                             ^^^^^ Selbstbezug
(define list-of
  (lambda (element)
    (signature (mixed empty-list (cons-list-of element)))))

; die leere Liste
(define-singleton empty-list ; Signatur
  empty empty?) 

; Ein Cons-Liste besteht aus:
; - erstes Element
; - Rest-Liste
(define-record (cons-list-of element)
  cons
  cons?
  (first element)
  (rest (list-of element)))

(define list-of-numbers
  (signature (list-of number)))

; 1elementige Liste: 5
(define list1 (cons 5 empty))

; 2elementige Liste: 5 8
(define list2 (cons 5 (cons 8 empty)))

; 3elementige Liste: 2 5 8
(define list3 (cons 2 (cons 5 (cons 8 empty))))

; 4elementige: 4 2 5 8
(define list4 (cons 4 list3))

(define list5 (cons 4 (cons 2 (cons 5 (cons 8 (cons 3 empty))))))

; Alle Elemente einer Liste addieren
(: list-sum (list-of-numbers -> number))

(check-expect (list-sum list4)
              19)

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
      ; x + 0 = 0 + x = x
      ((empty? list) 0) ; das neutrale Element der Addition
      ((cons? list)
       (+ (first list)
          (list-sum (rest list)))))))

; Alle Elemente einer Liste multiplizieren
(: list-product (list-of-numbers -> number))

(check-expect (list-product list4)
              320)

(define list-product
  (lambda (list)
    (cond
      ((empty? list) 1) ; neutrales Element der Multiplikation
      ((cons? list)
       (* (first list)
          (list-product (rest list)))))))

; Aus einer Liste die geraden Elemente extrahieren
(: extract-evens (list-of-numbers -> list-of-numbers))

(check-expect (extract-evens list4)
              (cons 4 (cons 2 (cons 8 empty))))


(define extract-evens
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (even? (first list))
           (cons (first list) (extract-evens (rest list)))
           (extract-evens (rest list)))))))

; Aus einer Liste die ungeraden Elemente extrahieren
(: extract-odds (list-of-numbers -> list-of-numbers))

(check-expect (extract-odds list4)
              (cons 5 empty))

(define extract-odds
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (odd? (first list))
           (cons (first list) (extract-odds (rest list)))
           (extract-odds (rest list)))))))

(define dillos (cons dillo1 (cons dillo2 empty)))

; Über Definitionen abstrahieren:
; - letztes Mal kopieren
; - umbenennen (rekursive Aufrufe!)
; - für die Unterschiede abstrakte Namen
; - abstrakte Namen in lambda aufnehmen (rekursive Aufrufe!)

; %element: Signaturvariable

(: extract ((%element -> boolean) (list-of %element) -> (list-of %element)))

(check-expect (extract even? list4)
              (cons 4 (cons 2 (cons 8 empty))))
                       
(define extract
  (lambda (p? list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (p? (first list))
           (cons (first list) (extract p? (rest list)))
           (extract p? (rest list)))))))

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
  (lambda (animals)
    (cond
      ((empty? animals) empty)
      ((cons? animals)
       (cons
        (run-over-animal (first animals))
        (run-over-animals (rest animals)))))))

; Alle Elemente einer Liste inkrementieren
(: inc-list ((list-of number) -> (list-of number)))

(check-expect (inc-list list4)
              (cons 5 (cons 3 (cons 6 (cons 9 empty)))))

(define inc-list
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons
        (inc (first list))
        (inc-list (rest list)))))))

(define inc (lambda (x) (+ 1 x)))