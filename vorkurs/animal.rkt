#lang deinprogramm/sdp/beginner
; Tiere auf dem texanischen Highway

; Gürteltier hat folgende Eigenschaften:  <- zusammengesetzte Daten
; - lebendig oder tot?
; - Gewicht
(define-record dillo
  make-dillo
  (dillo-alive? boolean)
  (dillo-weight number))

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

; Gilt nicht:
#;(check-property
 (for-all ((d dillo))
   (equal? (run-over-dillo d)
           d)))

(check-property
 (for-all ((d dillo))
   (not (dillo-alive? (run-over-dillo d)))))

#;(define run-over-dillo
    (lambda (dillo)
      (make-dillo ... ...)
      (dillo-alive? dillo)
      (dillo-weight dillo)))
      
(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))

; Gürteltier füttern
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 5)
              (make-dillo #t 15))
(check-expect (feed-dillo dillo2 5)
              dillo2)

#;(define feed-dillo
  (lambda (dillo amount)
    ; Fallunterscheidung
    ; zusammengesetzte Daten
    (define alive? (dillo-alive? dillo))
    (define weight (dillo-weight dillo))
    (make-dillo
     alive?
     (if alive?
         (+ weight amount)
         weight)
     #;(cond
       ((dillo-alive? dillo) (+ (dillo-weight dillo) amount))
       (else (dillo-weight dillo))))))

#;(define feed-dillo
  (lambda (dillo amount)
    (if (dillo-alive? dillo)
        (make-dillo #t (+ (dillo-weight dillo) amount))
        dillo)))

(define feed-dillo
  (lambda (dillo amount)
    (match dillo
      ((make-dillo #t weight) (make-dillo #t (+ weight amount)))
      ((make-dillo #f weight) dillo))))

; lexikalische Bindung:
; vom Vorkommen aus von innen nach außen suchen, nach lambda oder define
