#lang deinprogramm/sdp/beginner

; Ein Fluss ist eins der folgenden:
; - ein Bach
; - ein Zusammenfluss
; gemischte Daten
(define river
  (signature (mixed creek confluence)))

; Ein Bach hat folgende Eigenschaften
; - Ursprungsort
(define-record creek
  make-creek
  creek?
  (creek-origin string))

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
  (confluence-hauptfluss river)
  (confluence-nebenfluss river))

(define neckar1
  (make-confluence "Rottweil" eschach prim))

(define neckar2
  (make-confluence "Epfendorf" neckar1 schlichem))

; Fließt Wasser aus Ort in Fluß?
(: flows-from? (string river -> boolean))

(check-expect (flows-from? "Epfendorf" neckar2)
              #t)
(check-expect (flows-from? "Rottweil" neckar2)
              #t)
(check-expect (flows-from? "Dreifaltigkeitsberg" neckar2)
              #t)
(check-expect (flows-from? "Heimliswald" neckar2)
              #t)
(check-expect (flows-from? "Tübingen" neckar2)
              #f)

(define flows-from?
  (lambda (location river)
    ; abbruchbedingung?
    (cond
      ((creek? river)
       (string=? location (creek-origin river)))
      ((confluence? river)
       ... (confluence-location river) ...
       .. (confluence-hauptfluss river) ...
       ... (confluence-nebenfluss river) ...
       
       ))))
    