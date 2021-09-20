;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname river) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Ein Fluss:
; - kommt aus Quelle - ODER -
; - fließt aus 2 Flüssen zusammen

; Ein Fluss ist eins der folgenden:
; - ein Bach aus einer Quelle
; - ein Zusammenfluss von Hauptfluss und Nebenfluss
;                              ^^^^^          ^^^^^
; Selbstbezug
(define river
  (signature (mixed creek confluence)))

(define-record creek
  make-creek
  creek?
  (creek-origin string))

(define-record confluence
  make-confluence
  confluence?
  (confluence-location string)
  (confluence-main-stem 