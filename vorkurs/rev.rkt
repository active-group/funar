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
       (append-element
        (rev (rest list)) ; 4 3 2
        (first list) ; 1
      )))))


; Element an eine Liste anhängen
(: append-element ((list-of %a) %a -> (list-of %a)))

#;(check-expect (append-element (list 1 2 3) 4)
              (list 1 2 3 4))

(define append-element
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty))
      ((cons? list)
       (cons
        (first list)
        (append-element (rest list) element))))))


; Laufzeit: 1 + 2 + 3 + ... + (n-1) + n = (n+1)*n/2 = n^2 + ... = O(n^2)

(: rev-2 ((list-of %a) (list-of %a) -> (list-of %a)))

(check-expect (rev-2 (list 1 2 3 4) empty)
              (list 4 3 2 1))

(define rev-2
  (lambda (list acc) ; Akkumulator, enthält die bisher "gesehenen" Elemente umgedreht
    (cond
      ((empty? list) acc)
      ((cons? list)
       (rev-2 (rest list) (cons (first list) acc))))))