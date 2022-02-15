#lang deinprogramm/sdp

; Liste umdrehen
(: rev ((list-of %a) -> (list-of %a)))

#;(check-expect (rev (list 1 2 3 4))
              (list 4 3 2 1))

(define rev
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (add-element
        (rev (rest list)) ; 4 3 2
        (first list) ; 1
       )))))

(define rev*
  (lambda (list acc) ; acc die  "bisher gesehenen" Elemente, umgedreht
    (cond
      ((empty? list) acc)
      ((cons? list)
       ; kein Kontext!
       (rev* (rest list) (cons (first list) acc))))))

(rev* (list 1 2 3 4) empty)

; n + (n - 1) + ... + 2 + 1
; n*(n+1) / 2
; n^2 + ... = O(n^2)
(: add-element ((list-of %a) %a -> (list-of %a)))

#;(check-expect (add-element (list 1 2 3) 4)
              (list 1 2 3 4))

(define add-element
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty))
      ((cons? list)
       (cons
        (first list)
        (add-element (rest list) element))))))
                           