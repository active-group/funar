#lang deinprogramm/sdp/beginner
; Ein Fluss ist eins der folgenden:
; - Bach
; - Zusammenfluss
(define river
  (signature (mixed stream confluence)))

; Ein Bach hat folgende Eigenschaften:
; - Ursprungsort
(define-record stream
  make-stream stream?
  (stream-origin string))

(define eschach (make-stream "Heimliswald"))
(define prim (make-stream "Dreifaltigkeitsberg"))
(define schlichem (make-stream "Tieringen"))

; Ein Zusammenfluss hat folgende Eigenschaften:
; - Ort
; - Hauptfluss
; - Nebenfluss
;        ^^^^^ Selbstbezug
(define-record confluence
  make-confluence confluence?
  (confluence-location string)
  (confluence-main-stem river) ; Selbstbezug
  (confluence-tributary river)) ; Selbstbezug

(define neckar1 (make-confluence "Rottweil" eschach prim))
(define neckar2 (make-confluence "Epfendorf" neckar1 schlichem))