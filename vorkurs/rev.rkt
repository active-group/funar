#lang deinprogramm/sdp

; Liste umdrehen
(: rev ((list-of %a) -> (list-of %a)))

; Laufzeit 1 + 2 + 3 + ... + n = O(n^2)
; n*(n+1)/2
; Gaußsche Summenformel

#;(check-expect (rev (list 1 2 3 4))
              (list 4 3 2 1))

(define rev
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (append-element
        (rev (rest list)) ; 4 3 2
        (first list) ; 1
       )))))

; Element an eine Liste hängen
(: append-element ((list-of %a) %a -> (list-of %a)))

#;(check-expect (append-element (list 1 2 3) 4)
              (list 1 2 3 4))

(define append-element
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty))
      ((cons? list)
       (cons ; Kontext des rekursiven Aufrufs -> braucht Speicherplatz
        (first list)
        (append-element (rest list) element)))))) 


; Liste umdrehen, mit Zwischenergebnis
(: rev* ((list-of %a) (list-of %a) -> (list-of %a)))

(check-expect (rev* (list 1 2 3) empty)
              (list 3 2 1))

(define rev*
  (lambda (list acc) ; "Akkumulator"
    (cond
      ((empty? list) acc)
      ((cons? list)
       ; tail call / endrekursiver Aufruf
       (rev* (rest list) (cons (first list) acc)))))) ; kein Kontext, braucht keinen Speicherplatz


; gänginge Repräsentation für Kontext zur Laufzeit:
; Stack
; gängige Runtimes: Stack feste Größe, klein im Vergleich zum Hauptspeicher

; JVM: auch Tail-Calls verbrauchen Speicher
; => auf JVM-Sprachen gibt es spezielle Konstrukte für Endrekursion
; Kotlin: tailrec
; Scala: @tailrec
; Clojure: loop