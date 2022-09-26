#lang deinprogramm/sdp/beginner

; Tiere auf dem texanischen Highway

; Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot - UND -
; - Gewicht
; zusammengesetzte Daten
(define-record dillo
  make-dillo
  (dillo-alive? boolean)
  (dillo-weight number))

(: make-dillo (boolean number -> dillo))
(: dillo-alive? (dillo -> boolean))
(: dillo-weight (dillo -> number))

; lebendiges Gürteltier, 10kg
(define dillo1 (make-dillo #t 10))
; totes Gürteltier, 8kg
(define dillo2 (make-dillo #f 8))

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))
(check-expect (run-over-dillo dillo2)
              dillo2)

; Schablone
#;(define run-over-dillo
  (lambda (dillo)
    (make-dillo ... ...)
    ...
    (dillo-alive? dillo)
    (dillo-weight dillo)))

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))



; Absolutbetrag
(: absl (number -> number))

(check-expect (absl 5) 5)
(check-expect (absl -5) 5)

(define absl
  (lambda (n)
    ; binäre Verzweigung
    #;(cond
      ((>= n 0) n)
      (else (- n)))
    (if (>= n 0)
        n
        (- n))
    ))

; Gürteltier füttern
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 5)
              (make-dillo #t 15))
(check-expect (feed-dillo dillo2 5)
              dillo2)

(define feed-dillo
  (lambda (dillo amount)
    (define alive? (dillo-alive? dillo))
    (define weight (dillo-weight dillo))
    (make-dillo alive?
                (if alive?
                    (+ weight amount)
                    weight))))

  