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
