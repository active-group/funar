#lang deinprogramm/sdp

; Liste umdrehen
(: rev ((list-of %a) -> (list-of %a)))

; Anzahl der Aufrufe für Liste der Länge n:
; 1 + 2 + 3 + ... (n-1) + n
; n/2 * (n+1) = n^2 + ... = O(n^2)
; Gaußsche Summenformel

#;(check-expect (rev (list 1 2 3))
              (list 3 2 1))

(define rev
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (append-element ; Kontext vom Aufruf von rev
        (rev (rest list)) ; 3 2
        (first list) ; 1
        )))))

; bei Implementierungen mit Stack oft:
; - Stack ist in der Größe begrenzt
; - viel kleiner als der Hauptspeicher
; - auf der JVM verbrauchen auch endrekursive Aufrufe Platz auf dem Stack
; -> Clojure: loop, Kotlin: tailrec, Scala @tailrec


; Auch Liste umdrehen
(: rev2 ((list-of %a) (list-of %a) -> (list-of %a)))

(check-expect (rev2 (list 1 2 3) empty)
              (list 3 2 1))

(define rev2
  (lambda (list acc) ; acc: Akkumulator / Zwischenergebebnis
    (cond
      ((empty? list) acc)
      ((cons? list)
       ; kein Kontext: tail call / endrekursiver Aufruf
       (rev2 (rest list)
             (cons (first list) acc))))))

(: append-element ((list-of %a) %a -> (list-of %a)))

(define append-element
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty))
      ((cons? list)
       (cons
        (first list)
        (append-element (rest list) element))))))

; Elemente einer Liste aufsummieren, endrekursiv
(: list-sum2 ((list-of number) number -> number))

(check-expect (list-sum2 (list 1 2 3 4) 0)
              10)

; Schablone
#;(define list-sum2
  (lambda (list acc)
    (cond
      ((empty? list) acc)
      ((cons? list)
       (list-sum2 (rest list)
                  ... (first list) ... acc)))))

(define list-sum2
  ; acc ist die Summe der "bisher gesehenen Elemente"
  (lambda (list acc)
    (cond
      ((empty? list) acc)
      ((cons? list)
       (list-sum2 (rest list)
                  (+ (first list) acc))))))

; Aus einer Liste von Zahlen die geraden Zahlen extrahieren, endrekursiv