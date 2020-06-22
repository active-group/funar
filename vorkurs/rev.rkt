;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname rev) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Liste umdrehen
(: rev ((list-of %a) -> (list-of %a)))

#;(check-expect (rev (list 1 2 3))
              (list 3 2 1))

(define rev
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (add-to-end ; Kontext von (rev (rest list))
        (rev (rest list)) ; 3 2
        (first list)))))) ; 1

(define add-to-end
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty))
      ((cons? list)
       (cons(first list)
            (add-to-end (rest list) element))))))

; 5 + 4 + 3 + 2 + 1
; n/2 * (n-1) = n^2 ...


(check-expect (rev* (list 1 2 3))
              (list 3 2 1))

(define rev*
  (lambda (list)
    (rev-helper list empty)))

(define rev-helper
  (lambda (list rev-so-far)
    (cond
      ((empty? list) rev-so-far)
      ((cons? list)
       ; kein Kontext
       (rev-helper (rest list) (cons (first list) rev-so-far))))))