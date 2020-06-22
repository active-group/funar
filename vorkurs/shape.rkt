;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname shape) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))


; Zusammengesetzte Daten

; Ein Kreis hat folgende Eigenschaften:
; - Mittelpunkt
; - Radius
(define-record circle
  make-circle
  (circle-center point)
  (circle-radius real))

; Ein Punkt hat folgende Eigenschaften:
; Ein Punkt besteht aus:
; - X-Koordinate
; - Y-Koordinate
(define-record point
  make-point ; Konstruktor
  (point-x real) ; Selektor
  (point-y real)) ; Selektor

(: make-point (real real -> point))
(: point-x (point -> real))
(: point-y (point -> real))

(define point1 (make-point 5 10)) ; Punkt, X=5, Y=10
(define point2 (make-point 3 17)) ; Punkt, X=3, Y=17

