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
;        ^^^^^ Selbstbezug
(define-record confluence
  make-confluence confluence?
  (confluence-location string)
  (confluence-main-stem river)
  (confluence-tributary river))

(define prim (make-creek "Dreifaltigkeitsberg"))
(define eschach (make-creek "Heimliswald"))
(define schlichem (make-creek "Tieringen"))
(define neckar-1 (make-confluence "Rottweil" eschach prim))
(define neckar-2 (make-confluence "Epfendorf" neckar-1 schlichem))

; Fließt Wasser aus einem Ort in einen Fluss?
(: flows-from? (string river -> boolean))

(check-expect (flows-from? "Dreifaltigkeitsberg" prim)
              #t)
(check-expect (flows-from? "Dreifaltigkeitsberg" eschach)
              #f)
(check-expect (flows-from? "Heimliswald" neckar-1)
              #t)
(check-expect (flows-from? "Tübingen" neckar-2)
              #f)

; Schablone
#;(define flows-from?
  (lambda (location river)
    (cond
      ((creek? river)
       (creek-origin river)
       ...)
      ((confluence? river)
       ...
       (confluence-location river)
       (flows-from? location (confluence-main-stem river)) ; Selbstbezug
       (flows-from? location (confluence-tributary river)) ; Selbstbezug
       ...))))

(define flows-from?
  (lambda (location river)
    (cond
      ((creek? river)
       (string=? (creek-origin river) location))
      ((confluence? river)
       (or
        (string=? (confluence-location river) location)
        (flows-from? location (confluence-main-stem river)) ; Selbstbezug
        (flows-from? location (confluence-tributary river)) ; Selbstbezug
        )))))

