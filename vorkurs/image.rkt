;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname image) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
; Vorkurs Funktionale Softwarearchitektur
(define x (+ 23 42))
(define y
  (+ 23
     (* 42
        65)))

(define circle1 (circle 50 "solid" "red"))
(define rect1 (rectangle 100 50 "outline" "blue"))
(define square1 (square 100 "solid" "gold"))
(define star1 (star 50 "solid" "green"))
(define overlay1 (overlay star1 circle1))

#;(above
 (beside circle1 square1)
 (beside square1 circle1))

#;(above
 (beside square1 star1)
 (beside star1 square1))

; Abstraktion
; 1. ein letztes Mal kopieren
; 2. neue Namen für die Unterschiede
; 3. Namen in ein lambda übernehmen
; 4. den alten Code löschen

; Thema verfehlt:
;(define image1 square1)
;(define image2 star1)

(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))

(tile circle1 square1)