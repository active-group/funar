#lang deinprogramm/sdp/beginner
; Ein Fluss ist eins der folgenden:
; - Bach
; - Zusammenfluss

; Ein Bach hat folgende Eigenschaften:
; - Ursprungsort
(define-record stream
  make-stream stream?
  (stream-origin string))

(define eschach (make-stream "Heimliswald"))
(define prim (make-stream "Dreifaltigkeitsberg"))
(define schlichem (make-stream "Tieringen"))


