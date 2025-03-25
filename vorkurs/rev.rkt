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

; Liste Länge n
; 1 + 2 + 3 + ... + (n - 1) + n = n * (n + 1) / 2
; = O(n^2)


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


; mit Zwischenergebnis/Akkumulator
(: rev-2 ((list-of %a) (list-of %a) -> (list-of %a)))

(check-expect (rev-2 (list 1 2 3 4) empty)
              (list 4 3 2 1))

(define rev-2
  (lambda (list acc)
    (cond
      ((empty? list) acc)
      ((cons? list)
       (rev-2 (rest list)
              (cons (first list) acc))))))



















       