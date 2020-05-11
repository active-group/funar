;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname intro) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
(define mike "sperber")
(define pi 3.14159265)

(define s1 (star 50 "solid" "red"))
(define r1 (rectangle 200 100 "outline" "yellow"))
(define c1 (circle 50 "solid" "gold"))

(define b1 (beside s1 c1))

; (beside b1 (above s1 r1))

#;(above ; ganzen Ausdruck auskommentieren
 (beside s1 c1)
 (beside c1 s1))

(define sq1 (square 100 "solid" "green"))

#;(above
 (beside c1 sq1)
 (beside sq1 c1))

; Kachelmuster aus zwei Bildern machen
; 2stellige Funktion, 2 Images rein, 1 Image raus
(: tile (image image -> image))

(check-expect (tile s1 c1) (above
                            (beside s1 c1)
                            (beside c1 s1)))

(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))