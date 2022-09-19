#lang deinprogramm/sdp/beginner
; Ein Fluss ist eins der folgenden:
; - Bach (kommt aus Quelle)
; - Zusammentreffen (aus zwei Flüssen)
(define river
  (signature (mixed creek confluence)))

; Ein Bach hat folgende Eigenschaften:
; - Ursprungsort
(define-record creek
  make-creek creek?
  (creek-origin string))

; Ein Zusammentreffen hat folgende Eigenschaften:
; - Ort
; - Haupt*fluss*  <--- Selbstbezug
; - Nebenfluss    <--- auch
(define-record confluence
  make-confluence confluence?
  (confluence-location string)
  (confluence-main-stem river)
  (confluence-tributary river))

(define eschach (make-creek "Heimliswald"))
(define prim (make-creek "Dreifaltigkeitsberg"))
(define neckar1 (make-confluence "Rottweil" eschach prim))

