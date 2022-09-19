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
       (snoc
        (rev (rest list)) ; 3 2
        (first list) ; 1 
        )))))

; Element hinten an Liste anhängen
(: snoc ((list-of %a) %a -> (list-of %a)))

(check-expect (snoc (list 1 2 3) 4)
              (list 1 2 3 4))

(define snoc
  (lambda (list a)
    (cond
      ((empty? list) (cons a empty))
      ((cons? list)
       (cons
        (first list)
        (snoc (rest list) a))))))

