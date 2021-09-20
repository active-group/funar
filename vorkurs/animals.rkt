;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname animals) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Tiere auf dem texanischen Highway

; Datenanalyse

; Datendefinition
; Ein Gürteltier hat folgende Eigenschaften:
; Alternative: "besteht aus"
; - tot oder lebendig - UND -
; - Gewicht
; zusammengesetzte Daten
(define-record dillo
  make-dillo
  (dillo-liveness liveness)
  (dillo-weight number))

