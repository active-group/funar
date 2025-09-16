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
       (append-element ; Kontext
        (rev (rest list)) ; 4 3 2
        (first list) ; 1
       )))))

; rekursive Aufrufe für Liste der Länge n:
; 1 + 2 + 3 + ... + (n-1) + n = (n + 1) * n / 2 O(n^2)

; Element an Liste anhängen
(: append-element ((list-of %a) %a -> (list-of %a)))

#;(check-expect (append-element (list 4 3 2) 1)
              (list 4 3 2 1))

(define append-element
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty)) ; list steht im Schatten
      ((cons? list)
       (cons
        (first list) ; 4
        (append-element (rest list) element) ; 3 2 1
       )))))

; Liste umdrehen, 2. Version
(: rev2 ((list-of %a) (list-of %a) -> (list-of %a)))

(check-expect (rev2 (list 1 2 3 4) empty)
              (list 4 3 2 1))
(define rev2
  (lambda (list acc) ; acc: Zwischenergebnis, "die bisher gesehenen Elemente, umgredreht"
    (cond
      ((empty? list) acc)
      ((cons? list)
       (rev2 (rest list)
             ; neues Zwischenergebnis, aus dem alten Zwischenergebnis
             (cons (first list) acc))))))
                              