#lang deinprogramm/sdp

; Liste umdrehen
(check-expect (rev (list 1 2 3 4))
              (list 4 3 2 1))

(define rev
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (adjoin ; Kontext von rev-Aufruf
        (rev (rest list))
        (first list))))))

; O(n^2) = 1 + 2 + 3 + ... + (n-1) + n
; (n+1)*n/2 = n^2 + ...

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

(: rev2 ((list-of %a) (list-of %a) -> (list-of %a)))

(check-expect (rev2 (list 1 2 3 4) empty)
              (list 4 3 2 1))

(define rev2
  (lambda (list acc) ; Akkumulator
    (cond
      ((empty? list) acc)
      ((cons? list)
       (rev2 (rest list)
             (cons (first list) acc))))))
    
    
    