;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname animal) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Tiere auf dem texanischen Highway

; Ein Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot
; - Gewicht
; zusammengesetzte Daten
(define-record dillo
  make-dillo
  dillo? ; Prädikat
  (dillo-alive? boolean)
  (dillo-weight real))

(: make-dillo (boolean real -> dillo))
(: dillo? (any -> boolean))

(define dillo1 (make-dillo #t 12)) ; lebendiges Gürteltier, 12kg
(define dillo2 (make-dillo #f 10)) ; totes Gürteltier, 10kg

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 12))
(check-expect (run-over-dillo dillo2)
              dillo2)

#|
class Dillo {
   boolean alive;
   void runOver() {
     this.alive = false;
   }
}
|#

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))

; Gürteltier füttern
(: feed-dillo (dillo real -> dillo))

(check-expect (feed-dillo dillo1 3)
              (make-dillo #t 15))
(check-expect (feed-dillo dillo2 10) dillo2)

#;(define feed-dillo
  (lambda (dillo amount)
    (cond
      ((dillo-alive? dillo)
       (make-dillo #t (+ (dillo-weight dillo) amount)))
      (else dillo))))

(define feed-dillo
  (lambda (dillo amount)
    (define alive? (dillo-alive? dillo))
    (define weight (dillo-weight dillo))
    (make-dillo alive?
                (if alive?
                    (+ weight amount)
                    weight))))

; Ein Papagei hat folgende Eigenschaften:
; - Satz
; - Gewicht
(define-record parrot
  make-parrot
  parrot?
  (parrot-sentence string)
  (parrot-weight real))

(: make-parrot (string real -> parrot))

(define parrot1 (make-parrot "Hello!" 1))
(define parrot2 (make-parrot "Goodbye!" 0.5))

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot1)
              (make-parrot "" 1))

(define run-over-parrot
  (lambda (parrot)
    (make-parrot "" (parrot-weight parrot))))

; Ein Tier ist eins der folgenden:
; - Gürteltier
; - Papagei
; Fallunterscheidung
; gemischte Daten
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
      ((dillo? animal) (run-over-dillo animal))
      ((parrot? animal) (run-over-parrot animal)))))

; Gemischte Daten von zusammengesetzten Daten

; Eine Liste ist eins der folgenden:
; - die leere Liste
; - eine Cons-Liste aus erstem Element und Rest-Liste
;                                               ^^^^^ Selbstreferenz

(define list-of-numbers
  (signature (mixed empty-list
                    cons-list)))

(define-record empty-list
  make-empty
  empty?)

(define empty (make-empty))

; Eine Cons-Liste besteht aus:
; - erstes Element
; - Rest-Liste
(define-record cons-list
  cons
  cons?
  (first number)
  (rest list-of-numbers))

; 1elementige Liste: 5
(define list1 (cons 5 empty))

; 2elementige Liste: 7 5
(define list2 (cons 7 (cons 5 empty)))

; 3elementige Liste: 4 7 3
(define list3 (cons 4 (cons 7 (cons 3 empty))))

; 4elementige Liste: 8 4 7 3
(define list4 (cons 8 list3))

; Elemente einer Liste addieren
(: list-sum (list-of-numbers -> number))

(check-expect (list-sum list4) 22)

; Schablone

#;(define list-sum
  (lambda (list)
    (cond
      ((empty? list) ...)
      ((cons? list)
       (first list)
       (list-sum (rest list))
       ...))))

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0)
      ((cons? list)
       (+ (first list)
          (list-sum (rest list)))))))

; Alle Elemente einer Liste multiplizieren
(: list-product (list-of-numbers -> number))

(check-expect (list-product list4) 672)

(define list-product
  (lambda (list)
    (cond
      ((empty? list) 1) ; neutrales Element bzgl. *
      ((cons? list)
       (* (first list)
          (list-product (rest list)))))))