#lang deinprogramm/sdp/beginner

; Ein Fluss ist eins der folgenden:
; - ein Bach
; - ein Zusammenfluss

; Ein Bach hat folgende Eigenschaften
; - Ursprungsort
(define-record creek
  make-creek
  creek?
  (creen-origin string))

(define eschach (make-creek "Heimliswald"))
(define prim (make-creek "Dreifaltigkeitsberg"))
(define schlichem (make-creek "Tieringen"))

; Ein Zusammenfluss halt folgende Eigenschaften
; - ein Ort des Zusammenflusses
; - Hauptfluss
; - Nebenfluss
(define-record confluence
  make-confluence
  confluence?
  (confluence-location string)
  (confluence-hauptfluss creek)
  (confluence-nebenfluss creek))