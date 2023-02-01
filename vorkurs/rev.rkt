#lang deinprogramm/sdp

; Liste umdrehen
(: rev ((list-of %a) -> (list-of %a)))

; jetzt eingebaute Funktionen und Datentypen nutzen
; (map, fold, extract -> filter)
(check-expect (rev (list 1 2 3 4))
              (list 4 3 2 1))
(check-expect (rev empty)
              empty)

(define rev
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       ; ... (first list) ; 1
       (append-element (rev (rest list)) (first list) ; 4 3 2
                       )))))

; später gemacht:
#;(define rev
    (lambda (list)
      (rev* list empty)))

; Element hinten an eine Liste anhängen
(: append-element ((list-of %a) %a -> (list-of %a)))

(check-expect (append-element (list 1 2 3) 4)
              (list 1 2 3 4))
(check-expect (append-element empty 3)
              (list 3))

(define append-element
  (lambda (list element)
    (cond
      ((empty? list) (cons element list))
      ((cons? list)
       (cons
        (first list)
        (append-element (rest list) element))))))

(rev (list 1 2 3 4))

; 1 + 2 + 3 + 4 + ... + n
; n * (n + 1) / 2 = n^2 ...
; schlecht!

; mit Zwischenergebnis arbeiten
(: rev* ((list-of %a) (list-of %a) -> (list-of %a)))

(define rev*
  (lambda (list acc) ; Akkumulator / Zwischenergebns
    (cond
      ;; acc enthält die gesehenen Elemente, aber umgekehrt
      ((empty? list) acc)
      ((cons? list)
       ; tail call (endrekursiver Aufruf)
       (rev* (rest list)
             (cons (first list)
                   acc))))))

;(rev* (list 1 2 3) empty)
;(rev* (list 2 3) (cons 1 empty))
;(rev* ...)

; JVM: Stack sehr klein im Vergleich zum Heap
; - Tail calls verbrauchen leider auch Speicher
; => spezielle Konstrukte:
; Kotlin: tailrec
; Scala: @tailrec
; Clojure: loop / recur




