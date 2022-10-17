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


