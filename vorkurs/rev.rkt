;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname rev) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
; Liste umdrehen
(: rev ((list-of %a) -> (list-of %a)))

;(check-expect (rev (list 1 2 3)) (list 3 2 1))

(define rev
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (adjoin (rev (rest list)) ; 3 2
               (first list)))))) ; 1 

(: adjoin ((list-of %a) %a -> (list-of %a)))

;(check-expect (adjoin (list 3 2) 1) (list 3 2 1))

(define adjoin
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty))
      ((cons? list)
       (cons (first list)
             (adjoin (rest list) element))))))

; Berechnungsschritte: n + (n-1) + (n-2) + ... + 2 + 1
; n/2 * (n+1) = n^2 + ... = O(n^2)

; Programmieren mit Akkumulator

(: rev1 ((list-of %a) (list-of %a) -> (list-of %a)))

(check-expect (rev1 (list 1 2 3) empty) (list 3 2 1))

(define rev1
  (lambda (list acc)
    (cond
      ((empty? list) acc)
      ((cons? list)
       ; tail call, endrekursiver Aufruf
       (rev1 (rest list) (cons (first list) acc))))))
       

