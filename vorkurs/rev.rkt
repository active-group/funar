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
       (snoc ; Kontext von rev
        (rev (rest list)) ; 4 3 2
        (first list) ; 1
       )))))

; viele Runtimes benutzen für die Kette von Kontexten
; einen "Stack", linearer Bereich im Speicher,
; - feste Größe
; - klein (ca. 1000-10000 Einträge)

; Gauß'sche Summenformel
; 1 + 2 + 3 + ... + (n-1) + n = (n+1)*n/2 = O(n^2)

; Element hinten an eine Liste anhängen
(: snoc ((list-of %a) %a -> (list-of %a)))

(check-expect (snoc (list 1 2 3) 4)
              (list 1 2 3 4))

(define snoc
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty))
      ((cons? list)
       (cons (first list)
             (snoc (rest list) element))))))

; Liste umdrehen, 2. Versuch
(: rev-2 ((list-of %a) (list-of %a) -> (list-of %a)))

(check-expect (rev-2 (list 1 2 3 4) empty)
              (list 4 3 2 1))

(define rev-2
  (lambda (list acc)
    (cond
      ((empty? list) acc) ; Zwischenergebnis wird zum Endergebnis
      ((cons? list)
       (rev-2 (rest list)
              (cons (first list) acc))))))