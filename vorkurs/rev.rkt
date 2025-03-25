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
       (add-element ; Kontext vom rev-Aufruf
        (rev (rest list))
        (first list))))))

; Continuation
; (lambda (x) (add-element x (first list)))

; Liste Länge n
; 1 + 2 + 3 + ... + (n - 1) + n = n * (n + 1) / 2
; = O(n^2)


; Element an eine Liste anhängen
(: add-element ((list-of %a) %a -> (list-of %a)))

(check-expect (add-element (list 1 2 3) 4)
              (list 1 2 3 4))

(define add-element
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty))
      ((cons? list)
       (cons ; Kontext
        (first list)
        (add-element (rest list) element))))))

; JVM: Kontext werden auf Stack verwaltet
; - feste Größe
; - klein (~10.000 Kontexte)
; - auch tail calls verbrauchen Platz
;   ... aber: Kotlin tailrec, Scala @tailrec, Clojure loop

; mit Zwischenergebnis/Akkumulator
(: rev-2 ((list-of %a) (list-of %a) -> (list-of %a)))

(check-expect (rev-2 (list 1 2 3 4) empty)
              (list 4 3 2 1))

; Schleifeninvariante:
; acc enthält alle bisher gesehenen Elemente,
; nur umgedreht
(define rev-2
  (lambda (list acc)
    (cond
      ((empty? list) acc)
      ((cons? list)
       (rev-2 (rest list) ; tail call
              (cons (first list) acc))))))


; Liste aufsummieren

(check-expect (list-sum (list 7 3 4 5) (lambda (x) x))
              19)

; Continuation-Passing Style

(define list-sum
  (lambda (list k) ; k: Continuation
    (cond
      ((empty? list) (k 0)) ; neutrales Element von +
      ((cons? list)
       ; Kontext: (+ (first list) [Loch])
       (list-sum (rest list)
                 (lambda (result)
                   (k (+ (first list) result))))))))










       