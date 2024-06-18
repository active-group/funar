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
       (append-element ; Kontext des Aufrufs von rev
        (rev (rest list))
        (first list))))))

; Kontext zur Laufzeit: (meist) Stack, klein

; Laufzeit bei Länge n:
; 1 + 2 + ... + (n-1) + n
; von außen nach innen:
; 1 + n
; 2 + (n-1) = 1 + n
; ...
; = n/2 * (1+n) = (1+n) * n / 2 = (n + n^2)/2 = O(n^2)
(: append-element ((list-of %a) %a -> (list-of %a)))

#;(check-expect (append-element (list 1 2 3)
                              4)
              (list 1 2 3 4))

(define append-element
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty))
      ((cons? list)
       (cons ; Kontext von append-element
        (first list)
        (append-element (rest list) element))))))

(: rev2 ((list-of %a) (list-of %a) -> (list-of %a)))

(check-expect (rev2 (list 1 2 3) empty)
              (list 3 2 1))
(define rev2
  (lambda (list acc) ; Akkumulator: schon gesehene Elemente, umgedreht -> Zwischenergebnis
    (cond
      ((empty? list) acc)
      ((cons? list)
       (rev2 (rest list) ; kein Kontext! tail call, endrekursiver Aufruf
             (cons (first list) acc))))))

(: list-map2 ((%a -> %b) (list-of %a) (list-of %b) -> (list-of %b)))

(check-expect (list-map2 (lambda (x) (* x 2))
                         (list 1 2 3)
                         empty)
              (list 2 4 6))

(define list-map2
  ; Schleifeninvariante
  ; acc: Liste des Rückgabewerts von f der schon gesehenen Elemente
  (lambda (f list acc)
    (cond
      ((empty? list) (rev2 acc empty))
      ((cons? list)
       (list-map2 f (rest list)
                  (cons (f (first list)) acc))))))
