#lang racket
(define my-if
  (lambda (condition consequent alternative)
    (cond
      (condition consequent)
      (else alternative))))

(my-if #t 1 2)
(my-if #t 2 1)

(if #t 1 (/ 1 0))

(define-syntax my-if2
  (syntax-rules ()
    ((my-if2 condition consequent alternative)
     (cond
      (condition consequent)
      (else alternative)))))

(my-if2 #t 1 (/ 1 0))

(define-syntax my-or
  (syntax-rules ()
    ((my-or exp1 exp2)
     (let ((result1 exp1))
       (if result1
           result1
           exp2)))))
         
(define result1 17)

(my-or (+ 1 result1) 5)
