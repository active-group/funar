#lang deinprogramm/sdp/beginner

; Datenanalyse

; Datendefinition:

; Ein Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; Fallunterscheidung
; hier speziell Aufzählung
(define pet
  (signature (enum "dog" "cat" "snake")))

; Ist Haustier niedlich?
(: cute? (pet -> boolean))

(check-expect (cute? "dog") #t)
(check-expect (cute? "cat") #t)
(check-expect (cute? "snake") #f)

; Gerüst
#;(define cute?
  (lambda (pet)
    ...))

; Schablone
#;(define cute?
  (lambda (pet)
    (cond ; Fallunterscheidung => Verzweigung, 1 Zweig pro Fall
      ((string=? pet "dog") ...)
      ((string=? pet "cat") ...)
      ((string=? pet "snake") ...))))


(define cute?
  (lambda (pet)
    (cond ; Fallunterscheidung => Verzweigung, 1 Zweig pro Fall
      ((string=? pet "dog") #t)
      ((string=? pet "cat") #t)
      ((string=? pet "snake") #f))))

; lexikalische/statische Bindung
; von der Benutzung einer Variable von innen nach außen suchen
; die erste Definition/Bindung (lambda / define) ist zuständig.

; Uhrzeit besteht aus/hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten
; OOP: Klasse, Java: auch "Record", Kotlin: "data class"

; Eine Stunde ist eine ganze Zahl zwischen 0 und 23
(define hour (signature (integer-from-to 0 23)))
; Eine Minute ist eine ganze Zahl zwischen 0 und 59
(define minute (signature (integer-from-to 0 59)))
  
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour hour) ; Selektor / "Getter-Funktion"
  (time-minute minute))

(: make-time (hour minute -> time))
(: time-hour (time -> hour))
(: time-minute (time -> minute))


; 12 Uhr 24 Minuten
(define time1 (make-time 12 24))
; 14:32
(define time2 (make-time 14 32))

; Minuten seit Mitternacht
(: msm (time -> natural))

(check-expect (msm time1) 744)
(check-expect (msm time2) 872)

; Schablone:
#;(define msm
  (lambda (time)
    ... (time-hour time) ...
    ... (time-minute time) ...))

(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

; (natural -> time)

; Aus Minuten seit Mitternacht Zeit machen
(: msm->time (natural -> time))

(check-property
 (for-all ((t time))
   (expect (msm->time (msm t))
           t)))

(define msm->time
  (lambda (minutes)
    (make-time
     (quotient minutes 60)
     (modulo minutes 60))))

; Tiere auf dem texanischen Highway

; Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot? -UND-
; - Gewicht
; => zusammengesetzt
(define-record dillo
  make-dillo
  dillo?
  (dillo-alive? boolean)
  (dillo-weight number))

; Ist ein Wert ein Gürteltier?
(: dillo? (any -> boolean))

; lebendiges Gürteltier, 10kg
(define dillo1 (make-dillo #t 10))
; totes Gürteltier, 8kg
(define dillo2 (make-dillo #f 8))

; Gürteltier überfahren
#|
class Dillo {
  bool isAlive;
  double weight;

  void runOver() {
    this.isAlive = false;
  }
}
|#

(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))
(check-expect (run-over-dillo dillo2)
              dillo2)

(check-property
 (for-all ((d dillo))
   (expect (dillo-alive? (run-over-dillo d))
           #f)))

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))

; Gürteltier füttern
(: feed-dillo (dillo number -> dillo))

(check-expect
 (feed-dillo dillo1 2)
 (make-dillo #t 12))
(check-expect
 (feed-dillo dillo2 2)
 dillo2)

(define feed-dillo
  (lambda (dillo amount)
    (define alive? (dillo-alive? dillo))
    (define weight (dillo-weight dillo))
    (make-dillo alive?
                ; binäre Verzweigung
                (if alive?
                    (+ weight amount)
                    weight)
                #;(if (dillo-alive? dillo)
                    (+ (dillo-weight dillo) amount)
                    (dillo-weight dillo))
                #;(cond
                  ((dillo-alive? dillo)
                   (+ (dillo-weight dillo) amount))
                  (else
                   (dillo-weight dillo))))))


; Ein Papagei hat folgende Eigenschaften:
; - Satz
; - Gewicht
(define-record parrot
  make-parrot
  parrot?
  (parrot-sentence string)
  (parrot-weight number))

(: parrot? (any -> boolean))

; Begrüßungs-Papagei
(define parrot1 (make-parrot "Hello!" 1)) 

; Verabschiedungs-Papagei
(define parrot2 (make-parrot "Goodbye!" 2))

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot1)
              (make-parrot "" 1))

(define run-over-parrot
  (lambda (parrot)
    (make-parrot "" (parrot-weight parrot))))

; Ein Tier ist eins der folgenden:
; - Gürteltier -ODER-
; - Papagei
; Fallunterscheidung
; hier: gemischte Daten
(define animal
  (signature (mixed dillo parrot)))

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

; Duschprodukte

; - Seife (hat pH-Wert)
; - Shampoo (hat Haartyp)

; Ein Duschgel besteht aus:
; - Seife
; - Shampoo

; 1. Datenanalyse + Beispiele
; Funktion, die den Seifenanteil ermittelt.

; 2. Mixtur, die aus zwei Duschprodukten besteht
; Funktion entsprechend anpassen.

(define-record soap
  make-soap
  soap?
  (soap-ph number))

; Ein Haartyp ist eins der folgenden:
; - normal
; - fettig
; - Schuppen
(define hairtype
  (signature (enum "normal" "oily" "dandruff")))

(define-record shampoo
  make-shampoo
  shampoo?
  (shampoo-hairtype hairtype))

(define-record shower-gel
  make-shower-gel
  shower-gel?
  (shower-gel-soap soap)
  (shower-gel-shampoo shampoo))

(define-record mixture
  make-mixture
  mixture?
  (mixture-product1 shower-product) ; Selbstbezug
  (mixture-product2 shower-product)) ; Selbtbezug

; Ein Duschprodukt ist eins der folgenden:
; - Seife
; - Shampoo
; - Mixtur
(define shower-product
  (signature (mixed soap shampoo mixture)))

; Seifenanteil eines Duschprodukts, normiert auf 1
(: shower-product-soap (shower-product -> number))

(define shower-product-soap
  (lambda (product)
    (cond
      ((soap? product) 1)
      ((shampoo? product) 0)
      ((mixture? product)
       (/
        (+ (shower-product-soap (mixture-product1 product))
           (shower-product-soap (mixture-product2 product)))
        2)))))


; Eine Liste ist eins der folgenden:
; - die leere Liste
; - eine Cons-Liste bestehend aus erstem Element und Rest-Liste
;                                                         ^^^^^ Selbstbezug
(define list-of ; parametrische Polymorphie
  (lambda (element)
    (signature (mixed empty-list
                      (cons-list-of element)))))

; Die leere Liste ist ... die einzige wahre leere Liste
(define-singleton empty-list ; Signatur
  empty ; einzig wahrer Wert
  empty?) ; Prädikat

; Eine Cons-Liste besteht aus:
; - erstes Element
; - Rest-Liste
(define-record (cons-list-of element) ; macht im Hintergrund ein lambda
  cons
  cons?
  (first element)
  (rest (list-of element))) ; Selbstbezug

; 1elementige Liste: 5
(define list1 (cons 5 empty))
; 2elementige Liste: 5 2
(define list2         (cons 5 (cons 2 empty)))
; 3elementige Liste: 4 5 2
(define list3 (cons 4 (cons 5 (cons 2 empty))))
; 4elementige Liste: 7 4 5 2
(define list4 (cons 7 list3))

(define list-of-numbers (signature (list-of number)))

; Elemente einer Liste aufsummieren
(: list-sum (list-of-numbers -> number))

(check-expect (list-sum list4)
              18)

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0) ; das neutrale Element der Addition
      ((cons? list)
       (+ (first list)
          (list-sum (rest list)))))))

; Elemente einer Liste aufmultiplizieren
(: list-product (list-of-numbers -> number))

(check-expect (list-product list4)
              280)

(define list-product
  (lambda (list)
    (cond
      ((empty? list) 1) ; das neutrale Element der Multiplikation
      ((cons? list)
       (* (first list)
          (list-product (rest list)))))))

; Liste rein, Liste aller ungeraden Zahlen raus
(: list-odds (list-of-numbers -> list-of-numbers))

(check-expect (list-odds list4)
              (cons 7 (cons 5 empty)))

(define list-odds
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (odd? (first list))
           (cons (first list) (list-odds (rest list)))
           (list-odds (rest list)))))))

; Abstraktion über zwei Definitionen
; - letztes Mal kopieren
; - umbenennen (wichtig: rekursive Aufrufe auch)
; - Unterschiede durch abstrakte Namen ersetzen
; - abstrake Namen in lambda unterbringen (wichtig: rekursive Aufrufe erweitern)

; Elemente einer Liste extrahieren, bei denen das Prädikat #t liefert
(: list-extract ((number -> boolean) list-of-numbers -> list-of-numbers))
; heißt i.d.R. filter

; Higher-Order-Funktion / Funktion höherer Ordnung
; - mehr als 1 Pfeil in der Signatur
; - Funktion, die eine Funktion als Argument akzeptiert oder als Ergebnis liefert.

(check-expect (list-extract even? list4)
              (cons 4 (cons 2 empty)))
(check-expect (list-extract odd? list4)
              (cons 7 (cons 5 empty)))

(define list-extract
  (lambda (p? list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (p? (first list))
           (cons (first list) (list-extract p? (rest list)))
           (list-extract p? (rest list)))))))


(define list-fold
  (lambda (neutral-element-of-operator operator list)
    (cond
      ((empty? list) neutral-element-of-operator)
      ((cons? list)
       (operator (first list)
                 (list-fold neutral-element-of-operator operator (rest list)))))))

(define list-xxx
  (lambda (for-empty for-cons list)
    (cond
      ((empty? list) for-empty)
      ((cons? list)
       (for-cons (first list)
                 (list-xxx for-empty for-cons (rest list)))))))

; Elemente einer Liste verdoppeln
(: list-double (list-of-numbers -> list-of-numbers))

(check-expect (list-double list4)
              (cons 14 (cons 8 (cons 10 (cons 4 empty)))))

(define list-double
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons
        (double (first list))
        (list-double (rest list)))))))

(define double
  (lambda (x)
    (* x 2)))

; Elemente einer Liste inkrementieren
(: list-inc (list-of-numbers -> list-of-numbers))

(check-expect (list-inc list4)
              (cons 8 (cons 5 (cons 6 (cons 3 empty)))))

(define list-inc
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons
        (inc (first list))
        (list-inc (rest list)))))))

(define inc
  (lambda (x)
    (+ 1 x)))

;(: list-map ((number -> number) list-of-numbers -> list-of-numbers))
; %element: Signaturvariable
(: list-map ((%a -> %b) (list-of %a) -> (list-of %b)))

(define list-map
  (lambda (f list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons
        (f (first list))
        (list-map f (rest list)))))))

(define highway (cons dillo1 (cons dillo2 (cons parrot1 (cons parrot2 empty)))))