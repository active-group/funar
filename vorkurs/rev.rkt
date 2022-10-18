#lang deinprogramm/sdp

; Liste umdrehen
(: rev ((list-of %a) -> (list-of %a)))

#;(check-expect (rev (list 1 2 3 4))
              (list 4 3 2 1))

; Laufzeit 1 + 2 + 3 + 4 (jeweils Aufrufe von add-element)
; allgemein: 1 + 2 + .... + (n-1) + n
; = ((n + 1) * n) / 2 = n^2 + ... = O(n^2) 

(define rev
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (add-element ; Kontext => muß zur Laufzeit gespeichert werden: Continuation
        (rev (rest list)) ; 4 3 2
        (first list) ; 1
       )))))


; Element an Liste hinten dranhängen
(: add-element ((list-of %a) %a -> (list-of %a)))

#;(check-expect (add-element (list 1 2 3) 4)
              (list 1 2 3 4))

(define add-element
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty))
      ((cons? list)
       (cons
        (first list)
        (add-element (rest list) element))))))

(check-expect (rev* (list 1 2 3 4) empty) (list 4 3 2 1))

(define rev*
  (lambda (list acc) ; "Akkumulator" / Zwischenergebnis:
    ; Liste der bisher gesehenen Elemente, umgedreht
    (cond
      ((empty? list) acc)
      ((cons? list)
       ; tail call / endrekursiver Aufruf
       (rev* (rest list) ; hat keinen Kontext => benötigt zur Laufzeit keinen Speicher
             (cons (first list) acc))))))

; Java: Stack für Continuation
; - klein im Vergleich zum Hauptspeicher
; - auch tail calls verbrauchen Platz auf dem Stack

; => auf JVM: endrekursiv, + Konstrukte pro Programmiersprache
; Kotlin: tailrec
; Scala: @tailrec
; Clojure: loop

; Elemente einer Liste summieren
(: list-sum ((list-of number) -> number))

(check-expect (list-sum (list 1 2 3 4)) 10)

(define list-sum
  (lambda (list0)
    (define list-sum*
      ; Schleifeninvariante:
      ; acc ist die Summe der Elemente zwischen list0 und list
      (lambda (list acc)
        (cond
          ((empty? list) acc)
          ((cons? list)
           (list-sum* (rest list)
                      (+ acc (first list)))))))
    (list-sum* list0 0)))

    