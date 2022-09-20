#lang deinprogramm/sdp

#;(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0)
      ((cons? list)
       (+ (first list)
          (list-sum (rest list)))))))

; (list-sum (list 1 2 3 4))

(define list-sum
  (lambda (list0)    
    (define list-sum*
      ; acc ist die Summe aller Elemente zwischen list0 und list
      ; Schleifeninvariante
      (lambda (list acc) ; acc ist das Zwischenergebnis / die Summe der "gesehenen" Elemente
        (cond
          ((empty? list) acc)
          ((cons? list)
           ; Aufruf ohne Kontext / tail call / endrekursiv
           (list-sum* (rest list) (+ acc (first list)))))))
    (list-sum* list0 0)))
    

(list-sum (list 1 2 3 4))

