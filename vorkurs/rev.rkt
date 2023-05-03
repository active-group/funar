#lang deinprogramm/sdp

; Summe aller Listenelemente
(: list-sum ((list-of number) -> number))

;(check-expect (list-sum (list 1 2 3 4 5)) 15)

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0) ; neutrales Element bezüglich +
      ((cons? list)
       (+ (first list)
          (list-sum (rest list)))))))

; Liste umdrehen
(: rev ((list-of %a) -> (list-of %a)))

#;(check-expect (rev (list 1 2 3))
              (list 3 2 1))

(define rev
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (append-element
        (rev (rest list)) ; 3 2
        (first list) ; 1
       )))))

; Element an Liste hinten dranhängen
(: append-element ((list-of %a) %a -> (list-of %a)))

#;(check-expect (append-element (list 1 2 3) 4)
              (list 1 2 3 4))

(define append-element
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty))
      ((cons? list)
       (cons
        (first list)
        (append-element (rest list) element))))))

; Laufzeit für Liste der Länge n
; 1 + 2 + 3 + ... + (n-1) + n
; Gauß'sche Summenformel: (n+1)*n/2 = n^2 ...

; Schablone:
#;(define rev*
  ; acc besteht aus den "schon gesehenen Elementen", umgedreht
  (lambda (list acc)
    (cond
      ((empty? list) acc)
      ((cons? list)

       (rev* (rest list) ... (first list) ... acc)
       ...))))

(: rev* ((list-of %a) (list-of %a) -> (list-of %a)))

(check-expect (rev* (list 1 2 3) empty)
              (list 3 2 1))

(define rev*
  ; Schleifeninvariante:
  ; acc besteht aus den "schon gesehenen Elementen", umgedreht
  (lambda (list acc)
    (cond
      ((empty? list) acc)
      ((cons? list)
       ; kein Kontext!
       ; tail call
       ; endrekursiver Aufruf
       ; kein "Stack-Platz" notwendig
       ; außer auf der JVM :-(
       (rev* (rest list) (cons (first list) acc))))))
    