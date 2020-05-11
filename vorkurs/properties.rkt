;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname properties) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Funktionen haben Eigenschaften

; Eigenschaften sind testbar: QuickCheck


; neutrales Element

; Addition

(check-property
 (for-all ((x number))
   (and (= (+ x 0) x)
        (= (+ 0 x) x))))

(check-property
 (for-all ((x number))
   (and (= (* x 1) x)
        (= (* 1 x) x))))

; Kombinator
; (: op (a a -> a))

; (: + (number number -> number))

; Assoziativgesetz

(check-property
 (for-all ((a rational)
           (b rational)
           (c rational))
   (= (+ a (+ b c))
      (+ (+ a b) c))))

(define a #i-4.0)
(define b #i0.6666666666666666)
(define c #i0.6666666666666667)

; Kommutativgesetz
(check-property
 (for-all ((a number)
           (b number))
   (= (+ a b)
      (+ b a))))