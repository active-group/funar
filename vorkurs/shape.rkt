;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname shape) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
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

(define circle1 (make-circle point1 3)) ; Kreis mit Mittelpunkt X=5, Y=10, Radius 3
(define circle2 (make-circle point2 5)) ; Kreis mit Mittelpunkt point2, Radius 5

; Ist Punkt in Kreis?
(: in-circle? (point circle -> boolean))

(check-expect (in-circle? point1 circle1) #t)
(check-expect (in-circle? point2 circle2) #t)
(check-expect (in-circle? (make-point 20 20) circle1) #f)

; Schablone
#;(define in-circle?
  (lambda (point circle)
    (define center (circle-center circle))
    (point-x center)
    (point y center)
    (point-x point)
    (point-y point)
    (circle-center circle)
    (circle-radius circle)
    ...))

(define in-circle?
  (lambda (point circle)
    (<= (distance (circle-center circle) point)
        (circle-radius circle))))

(define non-negative?
  (lambda (x)
    (>= x 0)))

(define non-negative-number
  (signature (predicate
              non-negative?)))

; Abstand zwischen zwei Punkten berechnen
(: distance (point point -> (combined real
                                      non-negative-number)))

(check-expect (distance (make-point 0 0) (make-point 5 0))
              5)
(check-expect (distance (make-point 0 0) (make-point 0 5))
              5)
(check-expect (distance (make-point 0 0) (make-point 0 -5))
              5)

(define distance
  (lambda (point1 point2)
    (define dx (- (point-x point1)
                  (point-x point2)))

    (define dy (- (point-y point1)
                  (point-y point2)))

    (sqrt (+ (* dx dx) (* dy dy)))))

; Ein Quadrat hat folgende Eigenschaften:
; - eine untere linke Ecke
; - Seitenlänge
(define-record square
  make-square
  (square-ll-corner point)
  (square-side-length real))

(define square1 (make-square point1 5))
(define square2 (make-square point2 3))

(: in-square? (point square -> boolean))

(check-expect (in-square? point1 square1) #t)
(check-expect (in-square? point2 square2) #t)
(check-expect (in-square? (make-point 100 100) square2) #f)

#;(define in-square?
  (lambda (point square)
    (define ll-corner (square-ll-corner square))
    (and (>= (point-x point) (point-x ll-corner))
         (<= (point-x point) (+ (point-x ll-corner)
                                (square-side-length square)))
         (>= (point-y point) (point-y ll-corner))
         (<= (point-y point) (+ (point-y ll-corner)
                                (square-side-length square))))))
    
    
(define in-square?
  (lambda (point square)
    (define ll-corner (square-ll-corner square))
    (define px (point-x point))
    (define py (point-y point))
    (define llx (point-x ll-corner))
    (define lly (point-y ll-corner))
    (define side-length (square-side-length square))
    (and (>= px (point-x ll-corner))
         (<= px (+ llx side-length))
         (>= py lly)
         (<= py (+ lly side-length)))))
    

