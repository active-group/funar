;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname vorkurs2) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
; Ein Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot
; - Gewicht
(define-record-functions dillo
  make-dillo
  dillo? ; Prädikat
  (dillo-alive? boolean)
  (dillo-weight natural))
; "Zustand des Gürteltiers zu einem bestimmten Zeitpunkt"

#|
class Dillo {
  static boolean is(Object object) {
    return object instanceof Dillo;
  }
}

|#

(: make-dillo (boolean natural -> dillo))
(: dillo-alive? (dillo -> boolean))
(: dillo-weight (dillo -> natural))
(: dillo? (any -> boolean))

(define dillo1 (make-dillo #t 10)) ; Gürteltier, lebendig, 10kg
(define dillo2 (make-dillo #f 15)) ; Gürteltier, tot, 15kg

; Gürteltier überfahren
; class Dillo { ... void runOver() { this.alive = false } }
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1) (make-dillo #f 10))
(check-expect (run-over-dillo dillo2) dillo2)

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))


; (make-dillo (dillo-alive? d) (dillo-weight d)) = d

; Gürteltier füttern
(: feed-dillo (dillo -> dillo))

(check-expect (feed-dillo dillo1) (make-dillo #t 11))
(check-expect (feed-dillo dillo2) dillo2)

#;(define feed-dillo
  (lambda (dillo)
    (make-dillo ... ...)
    (if (dillo-alive? dill) ... ...)
    (dillo-alive? dillo)
    (dillo-weight dillo)))

#;(define feed-dillo
  (lambda (dillo)
    (make-dillo (dillo-alive? dillo)
                (if (dillo-alive? dillo)
                    (+ (dillo-weight dillo) 1)
                    (dillo-weight dillo)))))

(define feed-dillo
  (lambda (dillo)
    (if (dillo-alive? dillo)
        (make-dillo (dillo-alive? dillo)
                    (+ (dillo-weight dillo) 1))
        dillo
        #;(make-dillo (dillo-alive? dillo)
                    (dillo-weight dillo)))))

; Ein Papagei hat folgende Eigenschaften:
; - einen Satz, den er sagt
; - ein Gewicht
(define-record-functions parrot
  make-parrot
  parrot?
  (parrot-sentence string)
  (parrot-weight natural))

(define parrot1 (make-parrot "Der Schatz ist im Silbersee" 5)) ; Karl Mays Papagei, 5kg
(define parrot2 (make-parrot "Guten Tag!" 7)) ; höflicher Papagei, 7kg

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot1) (make-parrot "" 5))
(check-expect (run-over-parrot parrot2) (make-parrot "" 7))

(define run-over-parrot
  (lambda (parrot)
    (make-parrot "" (parrot-weight parrot))))

; Ein Tier ist eins der folgenden:
; - ein Gürteltier
; - ein Papagei
; gemischte Daten
(define animal
  (signature (mixed dillo parrot)))

; Tier überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal dillo1)
              (make-dillo #f 10))
(check-expect (run-over-animal parrot1)
              (make-parrot "" 5))


(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (run-over-dillo animal))
      ((parrot? animal) (run-over-parrot animal)))))

; Eine Liste von Zahlen ist eins der folgenden:
; - die leere Liste
; - ein Cons bestehend aus einer Zahl (erstes Element) und einer Liste von Zahlen (der Rest)
#;(: list-of (signature -> signature))
#;(define list-of
  (lambda (a)
    (signature (mixed empty-list
                      (cons-of a)))))

; Leere Liste besteht aus:
; <nix>
#;(define-record-functions empty-list
  make-empty
  empty?)

#;(define empty (make-empty))

; Ein Cons besteht aus:
; - Zahl (erstes Element)
; - Liste von Zahlen (Rest)
#;(define-record-functions (cons-of a)
  cons ; Konstruktor
  cons? ; Prädikat
  (first a)
  (rest (list-of a))) ; Selbstbezug

#;(: cons-of (signature -> signature))

(define list-of-number
  (signature (list-of number)))
  
(define list1 (cons 5 empty)) ; 1elementige Liste: 5
(define list2 (cons 7 (cons 5 empty))) ; 2elementige Liste: 7 5
(define list3 (cons 12 list2)) ; 3elementige Liste: 12 7 5

; Alle Elemente einer Liste addieren
(: list-sum (list-of-number -> number))

(check-expect (list-sum list1) 5)
(check-expect (list-sum list2) 12)
(check-expect (list-sum list3) 24)

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0)
      ((cons? list)
       (+
        (first list) ; erstes Element der Liste
        (list-sum (rest list)) ; Selbstbezug, Summe der restlichen Elemente
       )))))

; Elemente einer Liste multiplizieren
(: list-product (list-of-number -> number))

(check-expect (list-product list1) 5)
(check-expect (list-product list2) 35)
(check-expect (list-product list3) 420)

(define list-product
  (lambda (list) ; 12 7 5
    (cond
      ((empty? list) 1) ; neutrales Element
      ((cons? list)
       (* (first list) ; 12
          (list-product (rest list)) ; 35
          )))))

(: list-fold (%b (%a %b -> %b) (list-of %a) -> %b))

(check-expect (list-fold 0 + (list 1 2 3 4 5)) 15)
(check-expect (list-fold 1 * (list 1 2 3 4 5)) 120)
                         

(define list-fold
  (lambda (n op list)
    (cond
      ((empty? list) n)
      ((cons? list)
       (op (first list)
           (list-fold n op (rest list))
           )))))


; Positive Zahlen aus einer Liste extrahieren
(: list-positives (list-of-number -> list-of-number))

(check-expect (list-positives (cons 5 (cons -3 (cons -2 (cons 1 (cons 12 empty))))))
              (cons 5 (cons 1 (cons 12 empty))))

#;(define list-positives
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (positive? (first list))
           (cons
            (first list) ; 5
            (list-positives (rest list))) ; Liste: 1 12
           (list-positives (rest list)) 
       )))))

(define list-positives
  (lambda (list)
    (list-fold empty
               (lambda (first-list rec-result)
                 (if (positive? first-list)
                     (cons
                      first-list ; 5
                      rec-result) ; Liste: 1 12
                     rec-result))
               list)))

; Alle Elemente einer Liste extrahieren, die ein Prädikat erfüllen
(: list-filter ((%a -> boolean) (list-of %a) -> (list-of %a)))
   
(check-expect (list-filter positive? (cons 5 (cons -3 (cons -2 (cons 1 (cons 12 empty))))))
              (cons 5 (cons 1 (cons 12 empty))))
(check-expect (list-filter even? (cons 5 (cons 4 (cons 3 (cons 8 empty)))))
              (cons 4 (cons 8 empty)))

(: dl1 (list-of dillo))
(define dl1 (cons dillo1 (cons dillo2 empty)))

(check-expect (list-filter dillo-alive? dl1)
              (cons dillo1 empty))

(define list-filter
  (lambda (p? list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (p? (first list))
           (cons
            (first list)
            (list-filter p? (rest list)))
           (list-filter p? (rest list)) 
       )))))


; Alle Tiere auf dem Highway überfahren
(: run-over-animals ((list-of animal) -> (list-of animal)))

(check-expect (run-over-animals (list dillo1 parrot1 dillo2 parrot2))
              (list (run-over-animal dillo1)
                    (run-over-animal parrot1)
                    (run-over-animal dillo2)
                    (run-over-animal parrot2)))


(define run-over-animals
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons
        (run-over-animal (first list))
        (run-over-animals (rest list)))))))
         
(: list-map ((%a -> %b) (list-of %a) -> (list-of %b)))

(define list-map
  (lambda (f list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons
        (f (first list))
        (list-map f (rest list)))))))


; Element hinten an eine Liste hängen
(: list-adjoin ((list-of %a) %a -> (list-of %a)))

(check-expect (list-adjoin (list 1 2 3 4) 5)
              (list 1 2 3 4 5))

(define list-adjoin
  (lambda (list element) ; list: 1 2 3 4, element: 5
    (cond
      ((empty? list) (cons element empty)) ; (cons element empty)
      ((cons? list)
       (cons
        (first list) ; 1
        (list-adjoin (rest list) element) ; 2 3 4 5
       )))))

; Liste umdrehen
(: rev ((list-of %a) -> (list-of %a)))

(check-expect (rev (list 1 2 3 4 5))
              (list 5 4 3 2 1))

#;(define rev
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (list-adjoin
        (rev (rest list)) ; 5 4 3 2
        (first list))))))

(define rev
  (lambda (list)
    (rev-1 list empty)))

(define rev-1
  (lambda (list result) ; result: bisher gesehene Elemente, schon umgedreht
    (cond
      ((empty? list) result)
      ((cons? list)
       (rev-1 (rest list) ; endrekursiver Aufruf, tail call
              (cons (first list) result))))))
       

