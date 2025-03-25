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
       (add-element
        (rev (rest list))
        (first list))))))

; Element an eine Liste anhängen
(: add-element ((list-of %a) %a -> (list-of %a)))

(check-expect (add-element (list 1 2 3) 4)
              (list 1 2 3 4))

(define add-element
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty))
      ((cons? list)
       (cons
        (first list)
        (add-element (rest list) element))))))






       