#lang deinprogramm/sdp/beginner

; Ein Fluss ist eins der Folgenden:
; - ein Bach
; - ein Zusammenfluss
(define river
  (signature (mixed creek confluence)))

; Ein Bach hat folgende Eigenschaften:
; - Ursprungsort
; (- Name?)
(define-record creek
  make-creek
  creek? ; <- Ist es ein Bach?
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
  (confluence-tributary river)) ; Selbstbezug

(define neckar1
  (make-confluence "Rottweil" eschach prim))
(define neckar2
  (make-confluence "Epfendorf" neckar1 schlichem))

; Fließt Wasser aus einem best. Ort in einen best. Fluss?
(: flows-from? (string river -> boolean))

(check-expect (flows-from? "Epfendorf" neckar2)
              #t)
(check-expect (flows-from? "Tieringen" neckar1)
              #f)

(define flows-from?
  (lambda (location river)
    (cond
      ((creek? river)
       (string=? (creek-origin river)
                 location))
      ((confluence? river)
       ... (confluence-lcation river)
       ... (confluence-main-stem river)
       ... (confluence-tributary river)))))




