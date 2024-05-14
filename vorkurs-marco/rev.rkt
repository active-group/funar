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

(: rev*-worker ((list-of %a) (list-of %a) -> (list-of %a)))

(check-expect (rev*-worker empty (list 1 2 3 4))
              (list 4 3 2 1))
(check-expect (rev*-worker empty empty) empty)

(define rev*-worker
  (lambda (acc list) ; Akkumulator
    (cond
      ((empty? list) acc)
      ((cons? list)
       (rev*-worker ; Endrekursion/tail recursion
        ; proper tail recursion
        (cons (first list) acc)
        (rest list))))))

#|
Haskell:
foldr :: (a -> b -> b) -> b -> [a] -> b
foldl :: (b -> a -> b) -> b -> [a] -> b
|#

(: list-sum ((list-of number) -> number))

(check-expect (list-sum (list 1 2 3 4)) 10)

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0)
      ((cons? list)
       (+ (first list)
          (list-sum (rest list)))))))

; Kontext von list-sum
; (+ (first list)
;    [])
; Repräsentation des Kontext als Funktion:
; (lambda (x) (+ (first list) x))

(define list-sum*
  (lambda (list k) ; <-- Kontext des Funktionsaufrufs als Funktion
    ; k: Continuation, diese Art zu programmieren: CPS - Continuation-Passing Style
    (cond
      ((empty? list) (k 0))
      ((cons? list)
       (list-sum* (rest list)
                  (lambda (x) (k (+ (first list) x))))))))
           

; Schablone:
#;(define list-sum*-worker
  (lambda (list acc)
    (cond
      ((empty? list) acc)
      ((cons? list)
       (list-sum*-worker (rest list)
                         (...
                          (first list)
                          ...
                          acc
                          ...))))))

(check-expect (list-sum*-worker (list 1 2 3 4) 0)
              10)

(define list-sum*-worker
  ; Schleifeninvariante:
  ; acc ist die Summe aller bisher gesehenen Elemente
  (lambda (list acc)
    (cond
      ((empty? list) acc)
      ((cons? list)
       (list-sum*-worker (rest list)
                         (+
                          (first list)
                          acc))))))
    
  