#lang deinprogramm/sdp

; Aufgabe: Eine Liste umdrehen
(: rev ((list-of %a) -> (list-of %a)))

(check-expect (rev (list 1 2))
              (list 2 1))
(check-expect (rev empty) empty)

(define rev
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (append-element (reverse (rest list))
                       (first list))))))

; Ein Element an eine Liste hinten anhängen
(: append-element ((list-of %a) %a -> (list-of %a)))

(check-expect (append-element (cons 1 (cons 2 (cons 3 empty)))
                              4)
              (cons 1 (cons 2 (cons 3 (cons 4 empty)))))

(define append-element
  (lambda (list element)
    (cond
      ((empty? list) (cons element list))
      ((cons? list) (cons (first list)
                          (append-element (rest list)
                                          element))))))
             