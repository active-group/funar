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
       (append-element (rev (rest list)) ; 4 3 2
                       (first list); 1
       )))))

; Liste hat Länge n
; # der rekursiven Aufrufe:
; 1 + 2 + 3 ... + n
; Gaußsche Summenformel

; Element an eine Liste anhängen
(: append-element ((list-of %a) %a -> (list-of %a)))

(check-expect (append-element (list 4 3 2) 1)
              (list 4 3 2 1))

(define append-element
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty))
      ((cons? list)
       (cons (first list)  ; 4
             (append-element (rest list) element) ; 3 2 1
       )))))