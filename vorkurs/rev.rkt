;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname rev) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Liste umdrehen
(: rev ((list-of %a) -> (list-of %a)))

(check-expect (rev-iterative (list 1 2 3))
              (list 3 2 1))

(define rev
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (add-to-list (rev (rest list))
                    (first list))))))

; Element an Liste hinten anhängen
(: add-to-list ((list-of %a) %a -> (list-of %a)))

(define add-to-list
  (lambda (list element)
    (cond
      ((empty? list) (cons element empty))
      ((cons? list)
       (cons (first list)
             (add-to-list (rest list) element))))))


(define rev-iterative
  (lambda (list0)
    (rev2 list0 empty)))
  
(define rev2
  ; Schleifeninvariante
  ; result enthält die Elemente zwischen list0 und list
  ; in umgekehrter Reihenfolge
  (lambda (list result)
    (cond
      ((empty? list) result)
      ((cons? list)
       (rev2 (rest list) ; endrekursiver Aufruf / tail call
             (cons (first list) result))))))
  