;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname reverse) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Reihenfolge der Elemente einer Liste umdrehen
(: rev ((list-of %a) -> (list-of %a)))

#;(check-expect (rev (list 1 2 3))
              (list 3 2 1))

(define rev
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (append-element
        (rev (rest list))
        (first list))))))

(: append-element ((list-of %a) %a -> (list-of %a)))

#;(check-expect (append-element (list 3 2) 1)
              (list 3 2 1))

(define append-element
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty))
      ((cons? list)
       (cons
        (first list) ; 3
        (append-element (rest list) element))))))

; 1 + 2 + 3 + ... + (n-1) + n
; (n + 1) * n/2 = n^2 + ... = O(n^2)

; rev, aber mit Zwischenergebnis
(: rev1 ((list-of %a) (list-of %a) -> (list-of %a)))

(check-expect (rev1 (list 1 2 3) empty)
              (list 3 2 1))

; "acc ist die Liste der schon gesehenen Elemente, umgedreht."

(define rev1
  (lambda (list acc)
    (cond
      ((empty? list) acc)
      ((cons? list)
       (rev1 (rest list) (cons (first list) acc))))))
                    