#lang deinprogramm/sdp
; Liste umdrehen
(: rev ((list-of %a) -> (list-of %a)))

#;(check-expect (rev (list 1 2 3 4))
              (list 4 3 2 1))

; rev: 1 rekursiver Aufruf pro Listenelement
(define rev
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (append-element
        (rev (rest list)) ; 4 3 2
        (first list) ; 1
       )))))

; => 4 + 3 + 2 + 1 Funktionsaufrufe für Liste der Länge 4
; n + (n - 1) + ... + 2 + 1 Funktionsaufrufe für Liste der Länge n
; = ((n+1)*n)/2 = O(n^2)

; Element an Liste an Liste dranhängen
(: append-element ((list-of %a) %a -> (list-of %a)))

(check-expect (append-element (list 4 3 2) 1)
              (list 4 3 2 1))

(define append-element
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty))
      ((cons? list)
       (cons ; Kontext vom Aufruf append-element => zur Laufzeit Continuation
        (first list)
        (append-element (rest list) element))))))

; in vielen Runtimes: Stack als Repräsentation für die Continuation
       
