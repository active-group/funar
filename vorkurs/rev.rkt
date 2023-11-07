#lang deinprogramm/sdp

; Liste umdrehen
(: rev ((list-of %a) -> (list-of %a)))

(check-expect (rev (list 1 2 3 4))
              (list 4 3 2 1))

(define rev
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (append-element
        (rev (rest list)) ; 4 3 2
        (first list)))))) ; 1

; Element ans Ende einer Liste anhängen
(: append-element ((list-of %a) %a -> (list-of %a)))

(check-expect (append-element (list 1 2 3) 4)
              (list 1 2 3 4))

(define append-element
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty))
      ((cons? list)
       (cons
        (first list)
        (append-element (rest list) element))))))

#|
Laufzeitkomplexität bei Liste der Länge n:

Gauß'sche Summenformel:
1 + 2 + 3 + 4 ... + (n-2) + (n-1) + n = n*(n+1)/2 = O(n^2)

|#

(check-expect (rev2 (list 1 2 3 4) empty)
              (list 4 3 2 1))

(define rev2
  (lambda (list acc)
    (cond
      ((empty? list) acc)
      ((cons? list)
       (rev2 (rest list)
             (cons (first list) acc))))))