#lang deinprogramm/sdp/beginner

; Ein Fluss ist eins der folgenden:
; - ein Bach
; - ein Zusammenfluss
(define river
  (signature (mixed creek confluence)))

; Ein Bach hat folgende Eigenschaften:
; - Ursprungsort
(define-record creek
  make-creek
  creek?
  (creek-origin string))

(define eschach (make-creek "Heimliswald"))
(define prim (make-creek "Dreifaltigkeitsberg"))

