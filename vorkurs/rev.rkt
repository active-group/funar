;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname rev) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
(define list-product
  (lambda (list)
    (cond
      ; 1 is the identity of *
      ((empty? list) 1)
      ((cons? list)
       (* (first list)
          (list-product (rest list)))))))

(list-product (list 1 2 3 4))
