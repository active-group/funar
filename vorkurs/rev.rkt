#lang deinprogramm/sdp

; Liste umdrehen
(check-expect (rev (list 1 2 3 4))
              (list 4 3 2 1))

(define rev
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (adjoin
        (rev (rest list))
        (first list))))))

; Element hinten an Liste anhängen
(: adjoin ((list-of %a) %a -> (list-of %a)))

(check-expect (adjoin (list 1 2 3) 4)
              (list 1 2 3 4))

(define adjoin
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty))
      ((cons? list)
       (cons
        (first list)
        (adjoin (rest list) element))))))