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
(define schlichem (make-creek "Tieringen"))

; Ein Zusammenfluss hat folgende Eigenschaften:
; - Ort des Zusammenflusses
; - Hauptfluss
; - Nebenfluss
(define-record confluence
  make-confluence
  confluence?
  (confluence-location string)
  (confluence-main-stem river) ; Selbstbezug
  (confluence-tributary river))

(define neckar1
  (make-confluence "Rottweil" eschach prim))
(define neckar2
  (make-confluence "Epfendorf" neckar1 schlichem))

; Fließt Wasser aus Ort in Fluß?
(: flows-from? (string river -> boolean))

(check-expect (flows-from? "Epfendorf" neckar2) #t)
(check-expect (flows-from? "Tübingen" neckar2) #f)

(define flows-from?
  (lambda (location river)
    (cond
      ((creek? river)
       (string=? location (creek-origin river)))
      ((confluence? river)
       (or
        (string=? location (confluence-location river))
        (flows-from? location (confluence-main-stem river))
        (flows-from? location (confluence-tributary river)))))))