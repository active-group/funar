#lang deinprogramm/sdp

; Aufgabe: Eine Liste umdrehen
(: rev ((list-of %a) -> (list-of %a)))

(check-expect (rev (list 1 2 3 4))
              (list 4 3 2 1))
(check-expect (rev empty) empty)

(define rev
  (lambda (list)
    (cond
      ((empty? list) empty) ; oder list
      ((cons? list)
       (append-element ; ich wünsche eine funktion die das macht (Wunschdenken :))
        (rev (rest list))
        (first list))))))

; Hänge ein Elmenent _hinten_ an eine Liste an
(: append-element ((list-of %a) %a -> (list-of %a)))

(check-expect (append-element empty 42)
              (list 42))
(check-expect (append-element (list 1 2 3) 4)
              (list 1 2 3 4))

(define append-element
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty))
      ((cons? list)
       (cons
        (first list)
        (append-element (rest list) element))))))


; reverse aber besser
(: rev* ((list-of %a) -> (list-of %a)))


(check-expect (rev (list 1 2 3 4))
              (list 4 3 2 1))
(check-expect (rev empty) empty)

(define rev*
  (lambda (list)
    (rev*-worker empty list)))

(define rev*-worker
  (lambda (acc list) ; Akkumulator
    (cond
      ((empty? list) acc)
      ((cons? list)
       (rev*-worker
        (cons (first list) acc)
        (rest list))))))