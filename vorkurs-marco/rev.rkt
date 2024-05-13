#lang deinprogramm/sdp

; Aufgabe: Eine Liste umdrehen
(: rev ((list-of %a) -> (list-of %a)))

(check-expect (rev (list 1 2))
              (list 2 1))
(check-expect (rev empty) empty)

(define rev
  (lambda (list)
    (cond
      ((empty? list) empty) ; oder list
      ((cons? list)
       (append-element ; ich wünsche eine funktion die das macht (Wunschdenken :))
        (rev (cons-rest list))
        (cons-first list))))))
