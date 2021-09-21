;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname animals) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Tiere auf dem texanischen Highway

; Datenanalyse

; Datendefinition
; Ein Gürteltier hat folgende Eigenschaften:
; Alternative: "besteht aus"
; - tot oder lebendig - UND -
; - Gewicht
; zusammengesetzte Daten
(define-record dillo
  make-dillo
  dillo? ; Prädikat
  (dillo-liveness liveness)
  (dillo-weight number))

(: dillo? (any -> boolean))

; Eine "Lebendigkeit" ist eins der folgenden:
; - tot - ODER -
; - lebendig
; Fallunterscheidung
; hier Spezialfall: Aufzählung
(define liveness
  (signature (enum "dead" "alive")))

; Konstruktor
(: make-dillo (liveness number -> dillo))
; Selektor
(: dillo-liveness (dillo -> liveness))
(: dillo-weight (dillo -> number))
   
; lebendiges Gürteltier, 10kg
(define dillo1 (make-dillo "alive" 10))
; totes Gürteltier, 12kg
(define dillo2 (make-dillo "dead" 12))

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo "dead" 10))
(check-expect (run-over-dillo dillo2)
              dillo2)
(check-expect (dillo-liveness (run-over-dillo dillo1))
              "dead")

; property-based testing
(check-property
 (for-all ((d dillo))
   (string=? (dillo-liveness (run-over-dillo d))
             "dead")))

; Gerüst
#;(define run-over-dillo
  (lambda (dillo)
    ...))

; Schablone: zusammengesetzte Daten als Eingabe
#;(define run-over-dillo
  (lambda (dillo)
    ...
    (dillo-liveness dillo) ; nicht verwendet
    (dillo-weight dillo)
    ...
    ))

; Schablone: zusammengesetzte Daten als Ausgabe
#;(define run-over-dillo
  (lambda (dillo)
    (make-dillo ... ...)))

(define run-over-dillo
  (lambda (dillo)
    (make-dillo "dead" (dillo-weight dillo)))) 

; lexikalische Bindung
; von einem Vorkommen aus nach Bindung suchen
; 1. von innen nach außen nach lambda suchen
; 2. nach Definition suchen
; 3. muß eingebaut / importiert sein


; Gürteltier füttern
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 1)
              (make-dillo "alive" 11))
(check-expect (feed-dillo dillo2 1)
              dillo2)

(define feed-dillo
  (lambda (dillo amount)
    (define l (dillo-liveness dillo))
    (cond ; Verzweigung nach liveness
      ; Zweig: (<Bedingung> <Ergebnis>)
      ; die erste Bedingung, die #t ergibt, wird gezogen
      ((string=? "alive" l)
       (make-dillo l
                   (+ (dillo-weight dillo) amount)))
      ((string=? "dead" l)
       dillo))))

; Eine Klapperschlange hat folgende Eigenschaften:
; - Länge
; - Dicke
(define-record snake
  make-snake
  snake?
  (snake-length number)
  (snake-thickness number))

; 200cm lange Schlange, 5cm dick
(define snake1 (make-snake 200 5))
; Anakonda
(define snake2 (make-snake 500 10))

; Klapperschlange überfahren
(: run-over-snake (snake -> snake))

(check-expect (run-over-snake snake1) (make-snake 200 0))
(check-expect (run-over-snake snake2) (make-snake 500 0))

(define run-over-snake
  (lambda (snake)
    (make-snake (snake-length snake) 0)))

; Ein Tier ist eins der folgenden:
; - Gürteltier
; - Klapperschlange
; Fallunterscheidung
; speziell: gemischten Daten
(define animal
  (signature
   (mixed dillo snake)))

; Tier überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal dillo1)
              (run-over-dillo dillo1))
(check-expect (run-over-animal dillo2)
              (run-over-dillo dillo2))
(check-expect (run-over-animal snake1)
              (run-over-snake snake1))

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (run-over-dillo animal))
      ((snake? animal) (run-over-snake animal)))))

; A list is one of the following:
; - the empty list
; - a cons list consisting of first element and rest list
;                                                    ^^^^
;                                                    self-reference
(: list-of (signature  -> signature))
(define list-of
  (lambda (element)
    (signature (mixed empty-list
                      (cons-list-of element)))))

(define-record empty-list
  make-empty
  empty?)

(define empty (make-empty))

(define-record (cons-list-of element) ; makes a lambda
  ; (cons-list-of number)
  ; (cons-list-of animal)
  ; ...
  cons
  cons?
  (first element)
  (rest (list-of element))) ; <--- self-reference

(define list-of-numbers (signature (list-of number)))

; 1-element list: 17
(define list1 (cons 17 empty))
; 2-element list: 5 17
(define list2 (cons 5 (cons 17 empty)))
; 3-element list: 5 17 4
(define list3 (cons 5 (cons 17 (cons 4 empty))))
; 4-element list: 3 5 17 4
(define list4 (cons 3 list3))

; sum of the elements of a list
(: list-sum (list-of-numbers -> number))

(check-expect (list-sum list3) 26)

(define list-sum
  (lambda (list)
    (cond
      ; 0 is the identity of +
      ((empty? list) 0)
      ((cons? list)
       (+ (first list)
          (list-sum (rest list)))))))

; product of the elements of a list
(: list-product (list-of-numbers -> number))

(check-expect (list-product list3) 340)

(define list-product
  (lambda (list)
    (cond
      ; 1 is the identity of *
      ((empty? list) 1)
      ((cons? list)
       (* (first list)
          (list-product (rest list)))))))

(: list-fold (%b (%a %b -> %b) (list-of %a) -> %b))

(define list-fold
  (lambda (for-empty for-cons list)
    (cond
      ((empty? list) for-empty)
      ((cons? list)
       (for-cons (first list)
                 (list-fold for-empty for-cons (rest list)))))))

; extract the even numbers from a list
(: extract-evens (list-of-numbers -> list-of-numbers))

(check-expect (extract-evens
               (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 empty))))))
              (cons 2 (cons 4 (cons 6 empty))))

#;(define extract-evens
  (lambda (list)
    (cond
      ((empty? list) ...)
      ((cons? list)
       (first list)
       (extract-evens (rest list))
       ...))))

(define extract-evens
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (even? (first list))
           (cons (first list)
                (extract-evens (rest list)))
           (extract-evens (rest list)))
       #;(cond
         ((even? (first list))
          (cons (first list)
                (extract-evens (rest list))))
         (else
          (extract-evens (rest list))))))))

(define extract-odd
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (odd? (first list))
           (cons (first list)
                (extract-odd (rest list)))
           (extract-odd (rest list)))))))

; extract elements matching a predicate from list
; %element: signature variable
; Java <Element> ... m(...)
(: extract ((%element -> boolean) (list-of %element) -> (list-of %element)))

; Java: Stream<T> filter(Predicate<? super T> predicate)
; https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html
; Funtion<Integer, Bool>

; filter
(define extract
  (lambda (p? list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (p? (first list))
           (cons (first list)
                (extract p? (rest list)))
           (extract p? (rest list)))))))

; increment all the numbers in a list
(: inc-list (list-of-numbers -> list-of-numbers))

(check-expect (inc-list
               (cons 2 (cons 3 (cons 4 (cons 5 empty)))))
              (cons 3 (cons 4 (cons 5 (cons 6 empty)))))

(define inc-list
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons (inc (first list))
             (inc-list (rest list)))))))

(define inc (lambda (x) (+ x 1)))
(define dec (lambda (x) (- x 1)))

(define dec-list
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons (dec (first list))
             (dec-list (rest list)))))))

;(: list-map ((number -> number) (list-of number) -> (list-of number)))

(: list-map ((%a -> %b) (list-of %a) -> (list-of %b)))

(define list-map
  (lambda (f list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons (f (first list))
             (list-map f (rest list)))))))

;(list-map run-over-animal (cons dillo1 (cons dillo2 (cons snake1 empty))))

; Dependent Types: Idris, Agda
