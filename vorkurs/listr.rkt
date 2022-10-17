#lang deinprogramm/sdp/beginner
; Eine Liste1 ist eins der folgenden:
; - eine One-Liste
; - eine Cons1-Liste aus einem Element und Rest-Liste1
(define list1-of-numbers
  (signature (mixed one-list cons1-list)))

; Eine One-Liste besteht aus:
; - Element
(define-record one-list
  make-one one?
  (one-element number))

(define-record cons1-list
  cons1 cons1?
  (first1 number)
  (rest1 list1-of-numbers))

(define list1 (make-one 5))
(define list2 (cons1 2 (make-one 5)))
(define list3 (cons1 7 list2))
(define list4 (cons1 6 list3))

; Summe der Listenelemente berechnen
(: list1-sum (list1-of-numbers -> number))

(check-expect (list1-sum list4) 20)

(define list1-sum
  (lambda (list1)
    (cond
      ((one? list1) (one-element list1))
      ((cons1? list1)
       (+ (first1 list1)
          (list1-sum (rest1 list1)))))))
