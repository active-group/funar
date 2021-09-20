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
  dillo? ; Prädikat
  (dillo-liveness liveness)
  (dillo-weight number))

(: dillo? (any -> boolean))

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
    (define l (dillo-liveness dillo))
    (cond ; Verzweigung nach liveness
      ; Zweig: (<Bedingung> <Ergebnis>)
      ; die erste Bedingung, die #t ergibt, wird gezogen
      ((string=? "alive" l)
       (make-dillo l
                   (+ (dillo-weight dillo) amount)))
      ((string=? "dead" l)
       dillo))))

; Eine Klapperschlange hat folgende Eigenschaften:
; - Länge
; - Dicke
(define-record snake
  make-snake
  snake?
  (snake-length number)
  (snake-thickness number))

; 200cm lange Schlange, 5cm dick
(define snake1 (make-snake 200 5))
; Anakonda
(define snake2 (make-snake 500 10))

; Klapperschlange überfahren
(: run-over-snake (snake -> snake))

(check-expect (run-over-snake snake1) (make-snake 200 0))
(check-expect (run-over-snake snake2) (make-snake 500 0))

(define run-over-snake
  (lambda (snake)
    (make-snake (snake-length snake) 0)))

; Ein Tier ist eins der folgenden:
; - Gürteltier
; - Klapperschlange
; Fallunterscheidung
; speziell: gemischten Daten
(define animal
  (signature
   (mixed dillo snake)))

; Tier überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal dillo1)
              (run-over-dillo dillo1))
(check-expect (run-over-animal dillo2)
              (run-over-dillo dillo2))
(check-expect (run-over-animal snake1)
              (run-over-snake snake1))

(define run-over-animal
  (lambda (animal)
    (cond
      (... ...)
      (... ...))))
