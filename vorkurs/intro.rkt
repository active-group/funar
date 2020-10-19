;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname intro) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Vorkurs

; Pro Entität/Domänenobjekt gibt es Signatur

; Datenanalyse
; 1. Datendefinition: natürlichsprachige Beschreibung
; 2. Übersetzung in Code

; Ein Haustier ist eins der folgenden:
; - Hund ODER
; - Katze ODER
; - Schlange
; Fallunterscheidung
; Spezialfall: Aufzählung
(define pet
  (signature
   (enum "dog"
         "cat"
         "snake")))

; Ist ein Haustier niedlich?
(: cute? (pet -> boolean))

(check-expect (cute? "dog") #t)
(check-expect (cute? "cat") #t)
(check-expect (cute? "snake") #f)

; Gerüst
#;(define cute?
  (lambda (pet)
    ...))

; Schablone <- hängt ab von den Daten
#;(define cute?
  (lambda (pet)
    (cond
      (... ...)
      (... ...)
      (... ...))))

(define cute?
  (lambda (pet)
    (cond
      ((string=? pet "dog") #t)
      ((string=? pet "cat") #t)
      ((string=? pet "snake") #f))))

; Ein Tier (auf dem texanischen Highway) ist eins der folgenden:
; - Gürteltier
; - Papagei
; gemischte Daten
(define animal
  (signature
   (mixed dillo
          parrot)))

; Ein Gürteltier hat folgende Eigenschaften: / besteht aus:
; - tot oder lebendig UND
; - Gewicht
; zusammengesetzte Daten
(define-record dillo
  make-dillo
  (dillo-alive? boolean)
  (dillo-weight number))

; Konstruktor
(: make-dillo (boolean number -> dillo))
; Selektoren
(: dillo-alive? (dillo -> boolean))
(: dillo-weight (dillo -> number))

(define dillo1 (make-dillo #t 12)) ; Gürteltier, lebendig, 12kg
(define dillo2 (make-dillo #f 10)) ; Gürteltier, tot, 10kg

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 12))
(check-expect (run-over-dillo dillo2)
              #;(make-dillo #f 10) dillo2)

#;(define run-over-dillo
  (lambda (dillo)
    (make-dillo ... ...)
    
    ... (dillo-alive? dillo) ...
    ... (dillo-weight dillo) ...))


(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))

; Gürteltier füttern
(: feed-dillo (dillo -> dillo))

(check-expect (feed-dillo dillo1) (make-dillo #t 13))
(check-expect (feed-dillo dillo2) dillo2)

; (make-dillo (dillo-alive? dillo) (dillo-weight dillo)) = dillo

(define feed-dillo
  (lambda (dillo)
    (if (dillo-alive? dillo)
        (make-dillo (dillo-alive? dillo) (+ (dillo-weight dillo) 1))
        dillo #;(make-dillo (dillo-alive? dillo) (dillo-weight dillo)))
    #;(make-dillo (dillo-alive? dillo)
                (if (dillo-alive? dillo)
                    (+ (dillo-weight dillo) 1)
                    (dillo-weight dillo))
                #;(cond
                  ((dillo-alive? dillo) (+ (dillo-weight dillo) 1))
                  (else (dillo-weight dillo))))))
    

; Ein Papagei hat folgende Eigenschaften:
; - Satz
; - Gewicht
(define-record parrot
  make-parrot
  (parrot-sentence string)
  (parrot-weight number))

(define parrot1 (make-parrot "Hallo!" 2)) ; Begrüßungspapagei, 2kg
(define parrot2 (make-parrot "Tschüss!" 1)) ; andere Richtung, 1kg

(: make-parrot (string number -> parrot))
(: parrot-sentence (parrot -> string))
(: parrot-weight (parrot -> number))

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot1)
              (make-parrot "" 2))
(check-expect (run-over-parrot parrot2)
              (make-parrot "" 1))

(define run-over-parrot
  (lambda (parrot)
    (make-parrot "" (parrot-weight parrot))))

; Tier überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal dillo1)
              (run-over-dillo dillo1))
(check-expect (run-over-animal parrot1)
              (run-over-parrot parrot1))


    