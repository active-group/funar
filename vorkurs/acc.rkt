#lang deinprogramm/sdp

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0)
      ((cons? list)
       (+
        (first list)
        (list-sum (rest list)))))))

(list-sum (list 1 2 3 4))