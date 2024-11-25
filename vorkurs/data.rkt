#lang deinprogramm/sdp/beginner
; "Alles ist Daten."

; Datenanalyse

; Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; Fallunterscheidung
; hier: Aufzählung

; "dog", "cat", "snake"
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

; Schablone (ergibt sich ausschließlich aus der Signatur)
#;(define cute?
  (lambda (pet)
    ; Verzweigung
    ; einen Zweig pro Fall
    ; jeder Fall hat die Form (<Bedingung> <Ergebnis>)
    (cond
      ((equal? pet "dog") ...)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))

(define cute?
  (lambda (pet)
    ; Verzweigung
    ; einen Zweig pro Fall
    ; jeder Fall hat die Form (<Bedingung> <Ergebnis>)
    (cond
      ((equal? pet "dog") #t)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f))))


; Uhrzeit besteht aus / hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour (integer-from-to 0 23)) ; Selektor
  (time-minute (integer-from-to 0 59)))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; 11 Uhr 38 Minuten
(define time1 (make-time 11 38))

; 14:12
(define time2 (make-time 14 12))

; Minuten seit Mitternacht
(: msm (time -> natural))

(check-expect (msm time1)
              698)
(check-expect (msm time2)
              852)

; Schablone
#;(define msm
  (lambda (time)
    ... (time-hour time) ...
    ... (time-minute time) ...))

(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))


; Aus den Minuten seit Mitternacht die Uhrzeit berechnen
; msm->time
(: msm->time (natural -> time))

(check-expect (msm->time 698)
              time1)

; property-based testing
; QuickCheck
(check-property
 (for-all ((t time))
   (equal? (msm->time (msm t))
           t)))

; Schablone

#;(define msm->time
  (lambda (minutes)
    (make-time ... ...)))


(define msm->time
  (lambda (minutes)
    (make-time (quotient minutes 60)
               (modulo minutes 60))))


; Tiere auf dem texanischem Highway

; Gürteltier hat folgende Eigenschaften:
; - lebendig? -UND-
; - Gewicht
(define-record dillo
  make-dillo
  dillo? ; Prädikat (steht nach dem Konstruktor)
  (dillo-alive? boolean)
  (dillo-weight number))

(: make-dillo (boolean number -> dillo))
(: dillo-alive? (dillo -> boolean))
(: dillo-weight (dillo -> number))

(: dillo? (any -> boolean))

; lebendiges Gürteltier, 10kg
(define dillo1 (make-dillo #t 10))
; totes Gürteltier, 8kg
(define dillo2 (make-dillo #f 8))

; Gürteltier überfahren
(: roadkill-dillo (dillo -> dillo))

(check-expect (roadkill-dillo dillo1)
              (make-dillo #f 10))
(check-expect (roadkill-dillo dillo2)
              dillo2)

(define roadkill-dillo
  (lambda (dillo)
    (make-dillo #f
                (dillo-weight dillo))))

; Gürteltier füttern - tote Tiere fressen nix
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 2)
              (make-dillo #t 12))
(check-expect (feed-dillo dillo2 2)
              dillo2)

#;(define feed-dillo
  (lambda (dillo amount)
    (cond
      ((equal? (dillo-alive? dillo) #t)
       (make-dillo #t
                   (+ (dillo-weight dillo) amount)))
      ((equal? (dillo-alive? dillo) #f)
       dillo))))

#;(define feed-dillo
  (lambda (dillo amount)
    (make-dillo
     (dillo-alive? dillo)
     (cond
       ((dillo-alive? dillo)
        (+ (dillo-weight dillo) amount))
       ((not (dillo-alive? dillo))
        (dillo-weight dillo))))))

#;(define feed-dillo
  (lambda (dillo amount)
    (make-dillo
     (dillo-alive? dillo)
     (cond
       ((dillo-alive? dillo)
        (+ (dillo-weight dillo) amount))
       (else
        (dillo-weight dillo))))))

(define feed-dillo
  (lambda (dillo amount)
    (define alive? (dillo-alive? dillo))
    (define weight (dillo-weight dillo))
    (make-dillo
     alive?
     (if alive?
         (+ weight amount)
         weight))))


#|
class Dillo {
   void roadkill() { ... }
}
|#

; Papagei hat folgende Eigenschaften:
; - Satz -UND-
; - Gewicht
(define-record parrot
  make-parrot
  parrot?
  (parrot-sentence string)
  (parrot-weight number))

; Begrüßungspapagei
(define parrot1 (make-parrot "hello!" 1))

; dicker Verabschiedungspapagei
(define parrot2 (make-parrot "goodbye!" 2))

; Papagei überfahren
(: roadkill-parrot (parrot -> parrot))

(check-expect (roadkill-parrot parrot1)
              (make-parrot "" 1))

(define roadkill-parrot
  (lambda (parrot)
    (make-parrot ""
                 (parrot-weight parrot))))

; Tier ist eins der folgenden:
; - Gürteltier -ODER-
; - Papagei
; Fallunterscheidung
; jeder Fall hat eigene Datendefinition
; gemischte Daten
(define animal
  (signature (mixed dillo parrot)))

; Tier überfahren
(: roadkill-animal (animal -> animal))

(check-expect (roadkill-animal dillo1)
              (roadkill-dillo dillo1))
(check-expect (roadkill-animal parrot1)
              (roadkill-parrot parrot1))

(define roadkill-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (roadkill-dillo animal))
      ((parrot? animal) (roadkill-parrot animal)))))


; Eine Liste ist eins der folgenden:
; - die leere Liste -ODER-
; - eine Cons-Liste
;   bestehend aus erstem Element UND Rest-Liste
;                                         ^^^^^ Selbstbezug

(define list-of
  (lambda (element)
    (signature (mixed empty-list
                      (cons-list-of element)))))

(define-singleton empty-list ; Signatur
  empty ; Singleton-Wert
  empty?) ; Prädikat

(define-record (cons-list-of element) ; implizites lambda
  cons
  cons?
  (first element) ; vorläufig
  (rest (list-of element)))

; 1elementige Liste: 5
(define list1 (cons 5 empty))
; 2elementige Liste: 2 5
(define list2 (cons 2 (cons 5 empty)))
; 3elementige Liste: 2 5 8
(define list3 (cons 2 (cons 5 (cons 8 empty))))
; 4elementige Liste: 7 2 5 8
(define list4 (cons 7 list3))

; Liste aufsummieren
(: list-sum (list-of-numbers -> number))

(check-expect (list-sum list4)
              22)

; Schablone
#;(define list-sum
  (lambda (list)
    (cond
      ((empty? list) ...)
      ((cons? list)
       ...
       (first list)
       (list-sum (rest list))
       ...))))

; neutrales Element:
; 0 + x = x + 0 = x

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0) ; Jan: das neutrale Element der Addition
      ((cons? list)
       (+ (first list)
          (list-sum (rest list)))))))

; Liste aufmultiplizieren
(: list-product (list-of-numbers -> number))

(check-expect (list-product list4)
              560)

(define list-product
  (lambda (list)
    (cond
      ((empty? list) 1) ; Alex: das neutrale Element der Multiplikation
      ((cons? list)
       (* (first list)
          (list-product (rest list)))))))

; Aus einer Liste die ungeraden Elemente extrahieren
(: extract-odds (list-of-numbers -> list-of-numbers))

(check-expect (extract-odds list4)
              (cons 7 (cons 5 empty)))

(define extract-odds
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (odd? (first list))
           (cons (first list)
                 (extract-odds (rest list)))
           (extract-odds (rest list)))))))

; Abstraktion
; - kopieren (zum letzten Mal)
; - umbenennen (an rekursive Aufrufe denken)
; - Unterschiede durch abstrakte Namen ersetzen
; - Namen in lambda aufnehmen (an rekursiven Aufrufe denken)

; Higher-Order-Funktion: mehr als ein Pfeil
; Funktion höherer Ordnung
; eingebaut als filter
(: extract ((number -> boolean) list-of-numbers -> list-of-numbers))

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




      
