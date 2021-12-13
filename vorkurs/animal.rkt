;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname animal) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Tiere auf dem texanischen Highway

; Ein Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot
; - Gewicht
; zusammengesetzte Daten
(define-record dillo
  make-dillo
  (dillo-alive? boolean)
  (dillo-weight real))

(: make-dillo (boolean real -> dillo))

(define dillo1 (make-dillo #t 12)) ; lebendiges Gürteltier, 12kg
(define dillo2 (make-dillo #f 10)) ; totes Gürteltier, 10kg

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 12))
(check-expect (run-over-dillo dillo2)
              dillo2)

#|
class Dillo {
   boolean alive;
   void runOver() {
     this.alive = false;
   }
}
|#

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))
