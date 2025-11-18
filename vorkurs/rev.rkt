#lang deinprogramm/sdp

; Liste umdrehen
(: rev ((list-of %a) -> (list-of %a)))

#;(check-expect (rev (list 1 2 3 4))
              (list 4 3 2 1))

(define rev
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       ; Kontext vom Aufruf (rev (rest list))
       ; (append-element [] (first list))
       ; (lambda (hole) (append-element hole (first list))) ; Continuation
       (append-element (rev (rest list)) ; 4 3 2
                       (first list)))))) ; 1

; Liste der Länge n
; Laufzeit 1 + 2 + 3 + ... + (n-1) + n = (n+1)*n/2 = O(n^2)
; 

; Element an eine Liste anhängen
(: append-element ((list-of %a) %a -> (list-of %a)))

#;(check-expect (append-element (list 1 2 3) 4)
              (list 1 2 3 4))

(define append-element
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty)) ; list ist anderweitig gebunden
      ((cons? list)
       (cons (first list)
             (append-element (rest list) element))))))

; Liste umdrehen, Take #2
(: rev2 ((list-of %a) (list-of %a) -> (list-of %a)))

(check-expect (rev2 (list 1 2 3 4) empty)
              (list 4 3 2 1))

(define rev2
  (lambda (list acc) ; acc: alle vorher "gesehenen" Elemente, umgedreht
    (cond
      ((empty? list) acc)
      ((cons? list)
       ; rekursiver Aufruf: kein Kontext
       ; tail call, "endrekursiver Aufruf"
       (rev2 (rest list)
             (cons (first list) acc))))))

; JVM-Bug: Auch tail calls verbrauchen Platz auf dem Stack