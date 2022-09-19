#lang deinprogramm/sdp/beginner
; Ein Fluss ist eins der folgenden:
; - Bach (kommt aus Quelle) - ODER -
; - Zusammentreffen (aus zwei Flüssen)
(define river
  (signature (mixed creek confluence)))

; Ein Bach hat folgende Eigenschaften:
; - Ursprungsort
(define-record creek
  make-creek creek?
  (creek-origin string))

; Ein Zusammentreffen hat folgende Eigenschaften:
; - Ort - UND -
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

; Fließt Wasser aus einem Ort in einen Fluss?
(: flows-from? (string river -> boolean))

(check-expect (flows-from? "Heimliswald" eschach) #t)
(check-expect (flows-from? "Tübingen" eschach) #f)
(check-expect (flows-from? "Heimliswald" neckar1) #t)

; Schablone
#;(define flows-from?
  (lambda (location river)
    (cond
      ((creek? river)
       ... (creek-origin river) ...) ; Bach
      ((confluence? river)
       ... (confluence-location river) ...
       (flows-from? location (confluence-main-strem river)) ...
       (flows-from? location (confluence-tributary river)) ...)))) ; Zusammenfluss


(define flows-from?
  (lambda (location river)
    (cond
      ((creek? river)
       (string=? location (creek-origin river))) ; Bach
      ((confluence? river)
       (or (string=? location (confluence-location river))
           (flows-from? location (confluence-main-strem river))
           (flows-from? location (confluence-tributary river))))))) ; Zusammenfluss

