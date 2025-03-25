#lang deinprogramm/sdp/beginner

; Datenanalyse

; Datendefinition
; Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; Fallunterscheidung: Summe
; hier: Aufzählung
; -> Code
(define pet
  (signature (enum "dog" "cat" "snake")))

; Ist ein Haustier niedlich?
(: cute? (pet -> boolean))

(check-expect (cute? "cat") #t)
(check-expect (cute? "snake") #f)

; Schablone
#;(define cute?
  (lambda (pet)
    ; Verzweigung
    ; 1 Zweig pro Fall
    (cond
      ; Fall: (<Bedingung> <Ergebnis>)
      ((equal? pet "dog") ...)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))

(define cute?
  (lambda (pet)
    ; Verzweigung
    ; 1 Zweig pro Fall
    (cond
      ; Fall: (<Bedingung> <Ergebnis>)
      ((equal? pet "dog") #t)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f))))

; Uhrzeit besteht aus / hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten: Produkt
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour natural) ; Selektoren / "Getter-Funktionen"
  (time-minute natural))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; 11 Uhr 13 Minuten
(define time1 (make-time 11 13))
; 14:10
(define time2 (make-time 14 10))

; Minuten seit Mitternacht
(: msm (time -> natural))

(check-expect (msm time1)
              673)
(check-expect (msm time2)
              850)

; Schablone
#;(define msm
  (lambda (time)
    ... (time-hour time) ...
    ... (time-minute time) ...))


(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))


; msm->time

; zusammengesetzte Daten als Ausgabe:
; Konstruktor

; Tier (auf dem texanischen Highway):
; - Gürteltier -ODER-
; - Klapperschlange
; Fallunterscheidung / Summe
; gemischte Daten
(define animal
  (signature (mixed dillo snake)))

; Gürteltier hat folgende Eigenschaften:
; - (lebendig -ODER tot) -UND-
; - Gewicht
(define-record dillo
  make-dillo
  dillo? ; Prädikat
  (dillo-alive? boolean)
  (dillo-weight number))

(: dillo? (any -> boolean))

; "(Beschreibung von) Zustand des Gürteltiers zu
;  einem bestimmten Zeitpunkt"

(: make-dillo (boolean number -> dillo))

; lebendiges Gürteltier, 10kg
(define dillo1 (make-dillo #t 10))

; totes Gürteltier, 8kg
(define dillo2 (make-dillo #f 8))

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))
(check-expect (run-over-dillo dillo2)
              dillo2)

; QuickCheck: property-based testing
(check-property
 (for-all ((d dillo))
   (equal? #f
           (dillo-alive? (run-over-dillo d)))))

; Schablone
#;(define run-over-dillo
  (lambda (dillo)
    (make-dillo ... ...)
    ... (dillo-alive? dillo) ...
    ... (dillo-weight dillo) ...))
   
(define run-over-dillo
  (lambda (dillo)
    (if (> (dillo-weight dillo) 20)
        dillo
        (make-dillo #f (dillo-weight dillo)))))

; Gürteltier füttern - Menge variabel
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 5)
              (make-dillo #t 15))
(check-expect (feed-dillo dillo2 5)
              dillo2)


(define feed-dillo
  (lambda (dillo amount)
    (define alive? (dillo-alive? dillo))
    (define weight (dillo-weight dillo))
    (make-dillo alive?
                (cond                  
                  (alive?
                   (+ weight amount))
                  (else weight)))))

#;(define feed-dillo
  (lambda (dillo amount)
    ((lambda (alive? weight)
       (make-dillo alive?
                   (cond                  
                     (alive?
                      (+ weight amount))
                     (else weight))))
     (dillo-alive? dillo)
     (dillo-weight dillo))))

; lexikalische Bindung:
; vom Vorkommen des Namens von innen nach außen suchen
; - lambda oder
; - define
; Der erste Fund ist die Bindung.
; ... oder importiert.

; Klapperschlange hat folgende Eigenschaften:
; - Länge -UND-
; - Dicke
; zusammengesetzte Daten/Produkt
(define-record snake
  make-snake
  snake?
  (snake-length number)
  (snake-thickness number))

; Klapperschlange, 3m lang, 10cm dick
(define snake1 (make-snake 300 10))

; Klapperschlange überfahren
(: run-over-snake (snake -> snake))

(check-expect (run-over-snake snake1)
              (make-snake 300 0))

(define run-over-snake
  (lambda (snake)
    (make-snake (snake-length snake) 0)))

; Tier überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal dillo1)
              (run-over-dillo dillo1))
(check-expect (run-over-animal snake1)
              (run-over-snake snake1))

; Schablone
#;(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) ...)
      ((snake? animal) ...))))

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal)
       (run-over-dillo animal))
      ((snake? animal)
       (run-over-snake animal)))))

; Eine Liste ist eins der folgenden:
; - die leere Liste -ODER-
; - eine Cons-Liste, bestehend aus
;    erstem Element -UND- Rest-Liste
;                              ^^^^^
;                           Selbstbezug
(define-record empty-list
  make-empty-list
  empty?)

(define empty (make-empty-list))

(define-record (cons-list-of element)
  cons
  cons?
  (first element)
  (rest (list-of element)))

(: list-of (signature -> signature))

(define list-of
  (lambda (element-signature)
    (signature (mixed empty-list
                      (cons-list-of element-signature)))))

(define list-of-numbers (signature (list-of number)))

; Liste 1 Element: 7
(define list1 (cons 7 empty))
; Liste 2 Element: 4 7
(define list2 (cons 4 (cons 7 empty)))
; Liste 3 Elemente: 4 7 5
(define list3 (cons 4 (cons 7 (cons 5 empty))))
; Liste 4 Elemente: 3 4 7 5
(define list4 (cons 3 list3))

; Liste aufsummieren
(: list-sum (list-of-numbers -> number))

(check-expect (list-sum list4)
              19)

; Schablone:
#;(define list-sum
  (lambda (list)
    (cond
      ((empty? list) ...)
      ((cons? list)
       ...
       (first list)
       (list-sum (rest list))
       ...))))
    
(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0) ; neutrales Element von +
      ((cons? list)
       (+
        (first list)
        (list-sum (rest list)))))))

; Liste aufmultiplizieren
(: list-product (list-of-numbers -> number))

(check-expect (list-product list4)
              420)

(define list-product
  (lambda (list)
    (cond
      ((empty? list) 1) ; neutrale Element von *
      ((cons? list)
       (*
        (first list)
        (list-product (rest list)))))))

; 0 + x = x + 0 = x
; 1 * x = x * 1 = x

; Alle ungeraden Zahlen aus einer Liste extrahieren

(: extract-odds (list-of-numbers -> list-of-numbers))

(check-expect (extract-odds list4)
              (cons 3 (cons 7 (cons 5 empty))))

(define extract-odds
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (odd? (first list))
           (cons (first list)
                 (extract-odds (rest list)))
           (extract-odds (rest list)))))))

; Abstraktion über 2 Definitionen:
; - letztes Mal kopieren
; - umbenennen (rekursive Aufrufe nicht vergessen)
; - Unterschiede durch abstrakte Namen ersetzen
; - Namen in lambda aufnehmen (rekursive Aufrufe ...)

; >1 Pfeil: Funktion höherer Ordnung
; Signaturvariable %
; (: extract ((number -> boolean) (list-of number)
;                               -> (list-of number)))

(: extract ((%element -> boolean) (list-of %element)
                                -> (list-of %element)))
            
(define extract
  (lambda (p? list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (p? (first list))
           (cons (first list)
                 (extract p? (rest list)))
           (extract p? (rest list)))))))

(define dillos (cons dillo1 (cons dillo2 empty)))

(extract dillo-alive? dillos)

; (mixed empty-list (cons-list-of dillo) (cons-list-of snake))
; Teilmenge von:
; (list-of (mixed dillo snake))

; alle Zahlen in einer Liste inkrementieren
(: inc-list ((list-of number) -> (list-of number)))

(check-expect (inc-list list4)
              (cons 4 (cons 5 (cons 8 (cons 6 empty)))))

(define inc-list
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons
        (inc (first list))
        (inc-list (rest list)))))))

(define inc (lambda (n) (+ 1 n)))

(: run-over-animals ((list-of animal) -> (list-of animal)))

(check-expect (run-over-animals
               (cons dillo1 (cons snake1 empty)))
              (cons (run-over-animal dillo1)
                    (cons (run-over-animal snake1)
                          empty)))

(define run-over-animals
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons
        (run-over-animal (first list))
        (run-over-animals (rest list)))))))

#;(: list-map
   ((%element -> %new) (list-of %element) -> (list-of %new)))
(: list-map
   ((%a -> %b) (list-of %a) -> (list-of %b)))
             
(check-expect (list-map (lambda (n) (+ 1 n)) list4)
              (cons 4 (cons 5 (cons 8 (cons 6 empty)))))

(define list-map
  (lambda (f list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons
        (f (first list))
        (list-map f (rest list)))))))


(: list-fold (%b (%a %b -> %b) (list-of %a)  -> %b))

(define list-fold
  (lambda (e c list)
    (cond
      ((empty? list) e)
      ((cons? list)
       (c (first list)
          (list-fold e c (rest list)))))))


(: extract-odds2 (list-of-numbers -> list-of-numbers))

(check-expect (extract-odds2 list4)
              (cons 3 (cons 7 (cons 5 empty))))


(define extract-odds2
  (lambda (list)
    (list-fold empty
               (lambda (first-list rec-result)
                 (if (odd? first-list)
                     (cons first-list
                           rec-result)
                     rec-result))
               list)))


; neutrales Element ...

; Algebra:
; - Menge(n)
; - Operationen
; - Gleichungen

; Menge/Signatur a
; (: op (a a -> a))

; Assoziativität
; (op x (op y z)) = (op (op x y) z)

; ^^^ Halbgruppe


; neutrales Element:
; (: neutral a)
; (op neutral x) = (op x neutral) = x


; Halbgruppe + neutrales Element: Monoid








 
