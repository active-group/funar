#lang deinprogramm/sdp

; Liste umdrehen
(: rev ((list-of %a) -> (list-of %a)))

#;(check-expect (rev (list 1 2 3))
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

; Laufzeit:
; n + (n - 1) + ... + 1
; Gaußsche Summenformel: O(n^2)

; Element hinten an Liste anhängen
(: snoc ((list-of %a) %a -> (list-of %a)))

#;(check-expect (snoc (list 1 2 3) 4)
              (list 1 2 3 4))

(define snoc
  (lambda (list a)
    (cond
      ((empty? list) (cons a empty))
      ((cons? list)
       (cons
        (first list)
        (snoc (rest list) a))))))

(check-expect (rev* (list 1 2 3) empty)
              (list 3 2 1))

(define rev*
  ; acc ist die umgedrehte Liste aller bisher gesehener Elemente
  (lambda (list acc)
    (cond
      ((empty? list) acc)
      ((cons? list)
       (rev* (rest list)
             (cons (first list) acc))))))
    