;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname shapes) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Eine geometrische Figur ist eins der folgenden:
; - ein Kreis
; - ein Quadrat
; - eine Überlagerung zweier geometrischer Figuren

; Aufgabe:
; 1. Datenanalyse
; 2. Funktion, die feststellt, ob ein gegebener Punkt
;    innerhalb oder außerhalb der Figur liegt
(define ^2
  (lambda (x)
    (* x x)))