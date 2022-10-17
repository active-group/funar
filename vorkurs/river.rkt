#lang deinprogramm/sdp/beginner

; Ein Fluss ist eins der folgenden:
; - ein Bach - ODER -
; - Zusammenfluss
(define river
  (signature (mixed creek confluence)))

; Ein Bach hat folgende Eigenschaft(en):
; - Ursprungsort
(define-record creek
  make-creek creek?
  (creek-origin string))

; Ein Zusammenfluss hat folgende Eigenschaften:
; - Ort
; - Hauptfluss
;        ^^^^^ Selbstbezug
; - Nebenfluss
(define-record confluence
  make-confluence confluence?
  (confluence-location string)
  (confluence-main-stem river)
  (confluence-tributary river))

; Fließt Wasser aus einem Ort in einen Fluss?