#lang deinprogramm/sdp/beginner
; Tiere auf dem texanischen Highway

; Gürteltier hat folgende Eigenschaften:  <- zusammengesetzte Daten
; - lebendig oder tot?
; - Gewicht
(define-record dillo
  make-dillo
  (dillo-alive? boolean)
  (dillo-weight number))
