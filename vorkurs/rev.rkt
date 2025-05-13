#lang deinprogramm/sdp

; Liste umdrehen
(: rev ((list-of %a) -> (list-of %a)))

(check-expect (rev (list 1 2 3))
              (list 3 2 1))

(define rev
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (add-element
        (rev (rest list)) ; 3 2
        (first list) ; 1
       )))))

(: add-element ((list-of %a) %a -> (list-of %a)))

(check-expect (add-element (list 3 2) 1)
              (list 3 2 1))

(define add-element
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty))
      ((cons? list)
       (cons
        (first list)
        (add-element (rest list) element))))))
  