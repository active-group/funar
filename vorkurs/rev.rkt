#lang deinprogramm/sdp

; Liste umdrehen
(: rev ((list-of %a) -> (list-of %a)))

#;(check-expect (rev (list 1 2 3))
              (list 3 2 1))

(define rev
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (add-element ; Kontext des Aufrufs von rev
        (rev (rest list)) ; 3 2
        (first list) ; 1
       )))))

; Kontext oft zur Laufzeit auf Stack verwaltet

; Problem: Stack ist auf vielen Laufzeitumgebungen a) begrenzt b) klein

; Laufzeit für Liste der Länge n
; 1 + 2 + ... + (n-1) + n = (n+1)*n/2 = O(n^2)

(: add-element ((list-of %a) %a -> (list-of %a)))

#;(check-expect (add-element (list 3 2) 1)
              (list 3 2 1))

(define add-element
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty))
      ((cons? list)
       (cons
        (first list)
        (add-element (rest list) element))))))


; Liste umdrehen, 2. Versuch, linear
(: rev-2 ((list-of %a) (list-of %a) -> (list-of %a)))
;                      ^^^^^ Zwischenergebnis

(check-expect (rev-2 (list 1 2 3)
                     empty)
              (list 3 2 1))

; Schablone
#;(define rev-2
  (lambda (list acc)
    (cond
      ((empty? list) acc)
      ((cons? list)
       (rev-2 (rest list)
              ... (first list) acc ...) ; neues Zwischenergebnis
       ))))

(define rev-2
  (lambda (list acc)
    (cond
      ((empty? list) acc)
      ((cons? list)
       (rev-2 (rest list) ; kein Kontext => "tail call", endrekursiv
              (cons (first list) acc) ; neues Zwischenergebnis
       )))))

; Bug in JVM: Auch tail calls verbrauchen Stack-Platz
; => spezielle Konstrukte in Clojure, Scala, Kotlin