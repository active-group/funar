#lang deinprogramm/sdp

; Summe aller Listenelemente
(: list-sum (list-of-numbers -> number))

(check-expect (list-sum (list 1 2 3 4 5)) 15)

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0) ; neutrales Element bezüglich +
      ((cons? list)
       (+ (first list)
          (list-sum (rest list)))))))

