#lang deinprogramm/sdp/beginner

; Datendefinition:
; Haustier ist eins der folgenden:
; - Hund  -ODER-
; - Katze -ODER-
; - Schlange
; Fallunterscheidung
; Aufzählung
(define pet
  (signature (enum "dog"
                   "cat"
                   "snake")))

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
    ; Verzweigung, 1 Zweig pro Fall
    ; (<Bedingung> <Ergebnis>)
    (cond
      ((equal? pet "dog") ...)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))

(define cute?
  (lambda (pet)
    ; Verzweigung, 1 Zweig pro Fall
    ; (<Bedingung> <Ergebnis>)
    #;(match pet
      ("dog" #t)
      ("cat" #t)
      ("snake" #f))
        
    (cond
      ((equal? pet "dog") #t)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f))))

; Uhrzeit besteht aus / hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten / Produkte
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour natural) ; Selektor / "Getter"
  (time-minute natural))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; 11 Uhr 1 Minute
(define time1 (make-time 11 1))

; 14:07
(define time2 (make-time 14 07))

; Minuten seit Mitternacht
(: msm (time -> natural))

(check-expect (msm time1)
              661)
(check-expect (msm time2)
              847)

; Schablone
#;(define msm
  (lambda (time)
    ...
    (time-hour time)
    (time-minute time)
    ...))


#;(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

(define msm
  (lambda (time)
    (define hour (time-hour time))
    (define minute (time-minute time))
    (+ (* 60 hour)
       minute)))

; lexikalische Bindung:
; von einem Vorkommen einer Variable
; von INNEN nach AUSSEN
; suchen nach: dem ersten lambda, define

; Aus den Minuten-seit-Mitternacht eine Uhrzeit machen

; (: time-from-minutes (natural -> time))


; Schablone für die Erzeugung zusammengesetzter Daten:
; Konstruktor aufrufen

; Tier auf dem texanischen Highway
; - Gürteltier -ODER-
; - Papagei
; Fallunterwscheidung, hier: gemischte Daten, Summe
(define animal
  (signature (mixed dillo
                    parrot)))


; Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot?  -UND-
; - Gewicht
; zusammengesetzte Daten
(define-record dillo ; Signatur
  make-dillo ; Konstruktor
  dillo? ; Prädikat
  (dillo-alive? boolean) ; Selektoren
  (dillo-weight number))

(: dillo? (any -> boolean))
               
; lebendiges Gürteltier, 10kg
(define dillo1 (make-dillo #t 10))
; tot, 8kg
(define dillo2 (make-dillo #f 8))

#|
class Dillo {
   bool alive;
   void setAlive(bool alive) {
      this.alive = alive;
   }
   void runOver() {
      this.alive = false;
   }
}
|#

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))

(check-expect (run-over-dillo dillo2)
              dillo2)

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))

; Gürteltier füttern, variable Futtermenge
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 5)
              (make-dillo #t 15))
(check-expect (feed-dillo dillo2 5)
              dillo2)

#;(define feed-dillo
  (lambda (dillo amount)
    (make-dillo (dillo-alive? dillo)
                (cond
                  ((equal? (dillo-alive? dillo) #t)
                   (+ (dillo-weight dillo)
                      amount))
                  ((equal? (dillo-alive? dillo) #f)
                   (dillo-weight dillo))))))

; (: x boolean)
; (equal? x #t) = x

(define feed-dillo
  (lambda (dillo amount)
    (define alive? (dillo-alive? dillo))
    (define weight (dillo-weight dillo))
    (make-dillo alive?
                (if alive?
                    (+ weight amount)
                    weight)
                #;(cond
                  (alive? ; (equal? (dillo-alive? dillo) #t)
                   (+ weight amount))
                  (else
                   weight)))))


; Papagei hat folgende Eigenschaften
; - Satz -UND-
; - Gewicht
(define-record parrot
  make-parrot
  parrot?
  (parrot-sentence string)
  (parrot-weight number))

(define parrot1 (make-parrot "Welcome!" 1))
(define parrot2 (make-parrot "Goodbye!" 2))

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot1)
              (make-parrot "" 1))

(define run-over-parrot
  (lambda (parrot)
    (make-parrot ""
                 (parrot-weight parrot))))


; Tier überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal dillo1)
              (make-dillo #f 10))
(check-expect (run-over-animal parrot1)
              (make-parrot "" 1))

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (run-over-dillo animal))
      ((parrot? animal) (run-over-parrot animal)))))


; Liste ist eins der folgenden:
; - die leere Liste -ODER-
; - eine Cons-Liste bestehend aus erstem Element und Rest-Liste
;                                                         ^^^^^ Selbstbezug

(define list-of
  (lambda (element)
    (signature (mixed empty-list
                      (cons-list-of element)))))


#;(define list-of-numbers
  (signature (mixed empty-list
                    cons-list)))

(define-singleton empty-list ; Signatur
  empty ; Singleton
  empty?)  ; Prädikat

;(: empty empty-list)
(: empty? (any -> boolean))

#|
(define-record empty-list
  make-empty
  empty?)

(define empty (make-empty))
|#

(define-record (cons-list-of element) ; macht lambda
  cons
  cons?
  (first element)
  (rest (list-of element)))

#;(define-record cons-list
    cons
    cons?
    (first number)
    (rest list-of-numbers))
    

; 1elementigen Liste: 5
(define list1 (cons 5 empty))
; 2elementige Liste: 5 2
(define list2 (cons 5 (cons 2 empty)))
; 3elementige Liste: 5 2 7
(define list3 (cons 5 (cons 2 (cons 7 empty))))
; 4elementige Liste: 4 5 2 7
; (define list4 (cons 4 (cons 5 (cons 2 (cons 7 empty)))))
(define list4 (cons 4 list3))

(define list-of-numbers (signature (list-of number)))

; Liste aufsummieren
(: list-sum (list-of-numbers -> number))

(check-expect (list-sum list4)
              18)

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
      ((empty? list) 0)
      ((cons? list)
       (+ (first list)
          (list-sum (rest list)))))))

; 0 ist das neutrale Element von +
; x + 0 = 0 + x = x

; Liste aufmultiplizieren
(: list-product (list-of-numbers -> number))

(check-expect (list-product list4)
              280)

(define list-product
  (lambda (list)
    (cond
      ((empty? list) 1)
      ((cons? list)
       (* (first list)
          (list-product (rest list)))))))

; 1 ist das neutrale Element von *
; x * 1 = 1 * x = x

; Aus einer Liste die ungeraden Elemente extrahieren

(: extract-odds (list-of-numbers -> list-of-numbers))

(check-expect (extract-odds list4)
              (cons 5 (cons 7 empty)))

(define extract-odds
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (define f (first list))
       (define r (extract-odds (rest list)))
       (if (odd? f)
           (cons f r)
           r)))))

; Abstrahieren:
; - eine letzte Kopie
; - umbenennen (an rekursive Aufrufe denken!)
; - Unterschiede durch abstrakte Namen ersetzen
; - Namen in lambda aufnehmen (an rekursive Aufrufe denken!)

(define dillos
  (cons dillo1 (cons dillo2 empty)))

; Elemente aus einer Liste extrahieren, die ein Kriterium erfüllen
; %element: Signaturvariable
(: extract ((%element -> boolean) (list-of %element) -> (list-of %element)))
; (: extract ((number -> boolean) list-of-numbers -> list-of-numbers))
; eingebaut als "filter"

; List<Integer>

(check-expect (extract even? list4)
              (cons 4 (cons 2 empty)))
(check-expect (extract odd? list4)
              (cons 5 (cons 7 empty)))

(define extract
  (lambda (p? list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (define f (first list))
       (define r (extract p? (rest list)))
       (if (p? f)
           (cons f r)
           r)))))

(define highway (cons dillo1
                      (cons dillo2
                            (cons parrot1
                                  (cons parrot2 empty)))))

; Tiere überfahren
(: run-over-animals ((list-of animal) -> (list-of animal)))

(check-expect (run-over-animals highway)
              (cons (run-over-animal dillo1)
                    (cons (run-over-animal dillo2)
                          (cons (run-over-animal parrot1)
                                (cons (run-over-animal parrot2)
                                      empty)))))

(define run-over-animals
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons
        (run-over-animal (first list))
        (run-over-animals (rest list)))))))