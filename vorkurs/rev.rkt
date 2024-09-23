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
       (snoc
        (rev (rest list)) ; 4 3 2
        (first list) ; 1
       )))))

; Gauß'sche Summenformel
; 1 + 2 + 3 + ... + (n-1) + n = (n+1)*n/2 = O(n^2)

; Element hinten an eine Liste anhängen
(: snoc ((list-of %a) %a -> (list-of %a)))

(check-expect (snoc (list 1 2 3) 4)
              (list 1 2 3 4))

(define snoc
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty))
      ((cons? list)
       (cons (first list)
             (snoc (rest list) element))))))
