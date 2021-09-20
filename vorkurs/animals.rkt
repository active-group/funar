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

; Eine "Lebendigkeit" ist eins der folgenden:
; - tot - ODER -
; - lebendig
; Fallunterscheidung
; hier Spezialfall: Aufzählung
(define liveness
  (signature (enum "dead" "alive")))

; Konstruktor
(: make-dillo (liveness number -> dillo))
; Selektor
(: dillo-liveness (dillo -> liveness))
(: dillo-weight (dillo -> number))
   
; lebendiges Gürteltier, 10kg
(define dillo1 (make-dillo "alive" 10))
; totes Gürteltier, 12kg
(define dillo2 (make-dillo "dead" 12))

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo "dead" 10))
(check-expect (run-over-dillo dillo2)
              dillo2)
(check-expect (dillo-liveness (run-over-dillo dillo1))
              "dead")

; property-based testing
(check-property
 (for-all ((d dillo))
   (string=? (dillo-liveness (run-over-dillo d))
             "dead")))

; Gerüst
#;(define run-over-dillo
  (lambda (dillo)
    ...))

; Schablone: zusammengesetzte Daten als Eingabe
#;(define run-over-dillo
  (lambda (dillo)
    ...
    (dillo-liveness dillo) ; nicht verwendet
    (dillo-weight dillo)
    ...
    ))

; Schablone: zusammengesetzte Daten als Ausgabe
#;(define run-over-dillo
  (lambda (dillo)
    (make-dillo ... ...)))

(define run-over-dillo
  (lambda (dillo)
    (make-dillo "dead" (dillo-weight dillo)))) 

; lexikalische Bindung
; von einem Vorkommen aus nach Bindung suchen
; 1. von innen nach außen nach lambda suchen
; 2. nach Definition suchen
; 3. muß eingebaut / importiert sein


; Gürteltier füttern
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 1)
              (make-dillo "alive" 11))
(check-expect (feed-dillo dillo2 1)
              dillo2)

(define feed-dillo
  (lambda (dillo amount)
    (cond ; Verzweigung nach liveness
      ; Zweig: (<Bedingung> <Ergebnis>)
      ; die erste Bedingung, die #t ergibt, wird gezogen
      ((string=? "alive" (dillo-liveness dillo))
       (make-dillo (dillo-liveness dillo)
                   (+ (dillo-weight dillo) amount)))
      ((string=? "dead" (dillo-liveness dillo))
       dillo))))

