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
; gemischte Daten: Spezialfall von Fallunterscheidung
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
  dillo?
  (dillo-alive? boolean)
  (dillo-weight number))

; Konstruktor
(: make-dillo (boolean number -> dillo))
; Selektoren
(: dillo-alive? (dillo -> boolean))
(: dillo-weight (dillo -> number))
; Prädikat
(: dillo? (any -> boolean))

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
  parrot?
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

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (run-over-dillo animal))
      ((parrot? animal) (run-over-parrot animal)))))

; Eine Liste ist eins der folgenden:
; - die leere Liste - ODER -
; - eine Cons-Liste bestehend aus erstem Element und Rest-Liste
;                                             Selbstbezug ^^^^^
(define list-of
 (lambda (element)
   (signature
    (mixed empty-list
           (cons-list-of element)))))

(define-record empty-list
  make-empty-list
  empty?) ; keine Felder

(define empty (make-empty-list))

; Eine Cons-Liste besteht aus:
; - erstes Element
; - Rest - auch wieder eine Liste
(define-record (cons-list-of element) ; (lambda (element) ...)
  cons
  cons?
  (first element)
  (rest (list-of element)))

(: cons-list-of (signature -> signature))

(define list-of-numbers (signature (list-of number)))


(define list1 (cons 5 empty)) ; 1elementige Liste: 5
(define list2 (cons 3 (cons 5 empty))) ; 2elementige Liste: 3 5
(define list3 (cons 7 (cons 3 (cons 5 empty)))) ; 3elementige Liste: 7 3 5
(define list4 (cons 17 list3)) ; 4elementig: 17 7 3 5

; Elemente einer Liste addieren
(: list-sum (list-of-numbers -> number))

(check-expect (list-sum list3) 15)

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0)
      ((cons? list)
       (+ (first list)
          (list-sum (rest list)))))))

(define list-product
  (lambda (list)
    (cond
      ((empty? list) 1)
      ((cons? list)
       (* (first list)
          (list-product (rest list)))))))

; (cons 1 (cons 2 (cons 3 empty)))
; (*    1 (*    2 (*    3 1)))

(: list-fold (%b (%a %b -> %b) (list-of %a) -> %b))

(define list-fold
  (lambda (x f list)
    (cond
      ((empty? list) x)
      ((cons? list)
       (f (first list)
          (list-fold x f (rest list)))))))

; Liste inkrementieren
(: inc-list (list-of-numbers -> list-of-numbers))

(check-expect (inc-list list3)
              (cons 8 (cons 4 (cons 6 empty))))


(define inc-list
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons (inc (first list))
             (inc-list (rest list)))))))

(define inc
  (lambda (n)
    (+ 1 n)))

(define inc-helper
  (lambda (first-list rec-rest)
    (cons (inc first-list)
          rec-rest)))

(define inc-list2
  (lambda (list)
    (list-fold empty
               (lambda (first-list rec-rest)
                 (cons (inc first-list)
                       rec-rest))
               list)))

; Liste "verdoppeln"
(: double-list (list-of-numbers -> list-of-numbers))

(check-expect (double-list list3)
              (cons 14 (cons 6 (cons 10 empty))))

(define double-list
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons (double (first list))
             (double-list (rest list)))))))

(define double
  (lambda (n)
    (* n 2)))

(define double-list2
  (lambda (list0)
    (list-fold empty
               (lambda (a b)
                 (cons (double a)
                       b))
               list0)))

; Funktion auf jedes Element einer Liste anwenden
; %element: Signaturvariable, wird bei jedem Aufruf "automatisch belegt"
(: map-list ((%a -> %b) (list-of %a) -> (list-of %b)))

;(: map-list ((number -> number) (list-of number) -> (list-of number)))

(check-expect (map-list double list3)
              (cons 14 (cons 6 (cons 10 empty))))

(define map-list
  (lambda (f list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons (f (first list))
             (map-list f (rest list)))))))