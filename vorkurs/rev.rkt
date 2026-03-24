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
       (add-element ; Kontext des rekursiven Aufrufs
        (rev (rest list))
        (first list))))))

; Gauß'sche Summenformel
; Liste Länge n
; 1 + 2 + ... + (n-1) + n
; = (n+1)*n/2 = O(n^2)


; Element an eine Liste anhängen
(: add-element ((list-of %a) %a -> (list-of %a)))

#;(check-expect (add-element (list 1 2 3)
                           4)
              (list 1 2 3 4))

(define add-element
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty))
      ((cons? list)
       (cons
        (first list)
        (add-element (rest list) element))))))


; 2. Versuch
(: rev2 ((list-of %a) (list-of %a) -> (list-of %a)))

(check-expect (rev2 (list 1 2 3 4) empty)
              (list 4 3 2 1))

(define rev2
  (lambda (list acc) ; acc: Liste der "bisher gesehenen Elemente", umgedreht
    (cond
      ((empty? list) acc)
      ((cons? list)
       (rev2 (rest list) ; tail call
             (cons (first list) acc))))))

; JVM: tail calls verbrauchen auch Platz
; Kotlin, Scala, Clojure: spezielle Konstrukture für endrekursive Funktionen             
                    
