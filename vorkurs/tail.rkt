;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname tail) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Liste umdrehen
(: revq ((list-of %a) -> (list-of %a)))

(check-expect (revq (list 1 2 3 4))
              (list 4 3 2 1))

; # rekursiven Aufrufe: grob 4 + 3 + 2 + 1
; n + (n - 1) + (n - 2) + (n - 3)
; 1 + 2 + 3 + 4 + 5
; n * (n - 1) / 2 = n^2 ...
; Gauß
; quadratisch
(define revq
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (adjoin-list
        (revq ; 4 3 2
         (rest list)) ; 2 3 4
        (first list)))))) ; 1

(: adjoin-list ((list-of %a) %a -> (list-of %a)))

(check-expect (adjoin-list (list 1 2 3) 4)
              (list 1 2 3 4))

(define adjoin-list
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty))
      ((cons? list)
       (cons (first list) ; 1
             (adjoin-list ; 2 3 4
              (rest list) ; 2 3
              element))))))

; Liste umdrehen
(: rev ((list-of %a) -> (list-of %a)))

(check-expect (rev (list 1 2 3 4))
              (list 4 3 2 1))

(define rev
  (lambda (list0)
    (rev* list0 empty)))

(define rev*
  (lambda (list acc) ; acc: "alle schon gesehenen Element, umgredreht"
    (cond
      ((empty? list) acc)
      ((cons? list)
       (rev* (rest list)
             (cons (first list) acc))))))
  