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
       (add-to-list
        (rev (rest list)) ; 4 3 2
        (first list) ; 1
       )))))

; Laufzeit für Liste der Länge n:
; 1 + 2 + 3 + ... + (n-1) + n = (n + 1) + (n + 1) ... = (n+1)*n/2
; = O(n^2)

; Element an eine Liste anhängen
(: add-to-list ((list-of %a) %a -> (list-of %a)))

(check-expect (add-to-list (list 1 2 3) 4)
              (list 1 2 3 4))

(define add-to-list
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty))
      ((cons? list)
       (cons
        (first list)
        (add-to-list (rest list) element))))))

(: rev2 ((list-of %a) (list-of %a) -> (list-of %a)))

(check-expect (rev2 (list 1 2 3 4) empty)
              (list 4 3 2 1))

(define rev2
  (lambda (list acc) ; Akkumulator, "bisher gesehene Elemente, umgedreht
    (cond
      ((empty? list) acc)
      ((cons? list)
       (rev2 (rest list) ; kein Kontext: tail call, endrekursiver Aufruf
             (cons (first list) acc))))))
                  