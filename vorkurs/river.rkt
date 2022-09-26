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

; Fließt Wasser von einem Ort in Fluss?
(: flows-from? (string river -> boolean))

(check-expect (flows-from? "Heimliswald" eschach) #t)
(check-expect (flows-from? "Tübingen" eschach) #f)
(check-expect (flows-from? "Heimliswald" neckar2) #t)

; Schablone:
#;(define flows-from?
  (lambda (location river)
    (cond
      ((stream? river)
       ...
       (stream-origin river)
       ...)
      ((confluence? river)
       ...
       (confluence-location river)
       (flows-from? location (confluence-main-stem river))
       (flows-from? location (confluence-tributary river))
       ...))))

 (define flows-from?
  (lambda (location river)
    (cond
      ((stream? river)
       (string=? location (stream-origin river)))
      ((confluence? river)
       (or
        (string=? (confluence-location river))
        (flows-from? location (confluence-main-stem river))
        (flows-from? location (confluence-tributary river)))))))
   