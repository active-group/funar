#lang deinprogramm/sdp/beginner
; Datenanalyse

; Ist Haustier niedlich?

; Datendefinition:
; Ein Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; Fallunterscheidung
; hier speziell: Aufzählung
(define pet
  (signature (enum "dog" "cat" "snake")))

;(: cute? ((enum "dog" "cat" "snake") -> boolean))

(: cute? (pet -> boolean))

(check-expect (cute? "dog") #t)
(check-expect (cute? "cat") #t)
(check-expect (cute? "snake") #f)
;(check-expect (cute? "mouse") #t)

; Gerüst
#;(define cute?
  (lambda (pet)
    ...))

; Schablone
#;(define cute?
  (lambda (pet)
    (cond ; Verzweigung
      ((equal? pet "dog") ...) ; (<Bedingung> <Ergebnis>)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))

(define cute?
  (lambda (pet)
    (cond ; Verzweigung
      ((equal? pet "dog") #t) ; (<Bedingung> <Ergebnis>)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f)
      #;(else #t)))) ; macht das nicht ...

; Uhrzeit besteht aus / hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten
(define-record time ; Signatur
  make-time ; Konstruktor
  (time-hour natural) ; Selektor / "Getter-Funktion"
  (time-minute natural))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; 11 Uhr 35
(define time1 (make-time 11 35))
; 14:11 Uhr
(define time2 (make-time 14 11))

; Minuten seit Mitternacht
(: msm (time -> natural))

(check-expect (msm time1)
              695)
(check-expect (msm time2)
              851)

(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

; (Minuten seit Mitternacht) rein -> Uhrzeit raus
(: msm-to-time (natural -> time))

(check-expect (msm-to-time 14)
              (make-time 0 14))

(define msm-to-time
  (lambda (minutes)
    (define hours (quotient minutes 60))
    (make-time
     hours
     (- minutes (* 60 hours))
     #;(remainder minutes 60))))


; Tier (auf dem texanischen Highway) ist eins der folgenden:
; - Gürteltier -ODER-
; - Papagei
; gemischte Daten
(define animal
  (signature (mixed dillo parrot)))

; Gürteltiere hat folgende Eigenschaften: 
; - lebendig oder tot? -UND-
; - Gewicht
; zusammengesetzte Daten
(define-record dillo
  make-dillo
  dillo? ; Prädikat
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
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))
(check-expect (run-over-dillo dillo2)
              dillo2)

; Schablone
#;(define run-over-dillo
  (lambda (dillo)
    (make-dillo ... ...)
    ... (dillo-alive? dillo) ...
    ... (dillo-weight dillo) ...))


(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))

; Gürteltier füttern (Futtermenge variabel)
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 5)
              (make-dillo #t 15))
(check-expect (feed-dillo dillo2 5)
              dillo2)

#;(define feed-dillo
  (lambda (dillo amount)
    (make-dillo
     ‘
     (cond
       ((dillo-alive? dillo)
        (+ (dillo-weight dillo) amount))
       ((not (dillo-alive? dillo))
        (dillo-weight dillo))))))

(define feed-dillo
  (lambda (dillo amount)
    (make-dillo
     (dillo-alive? dillo)
     (if (dillo-alive? dillo)
         (+ (dillo-weight dillo) amount)
         (dillo-weight dillo)))))

; Ein Papagei hat folgende Eigenschaften:
; - Satz
; - Gewicht
(define-record parrot
  make-parrot
  parrot?
  (parrot-sentence string)
  (parrot-weight number))

; Begrüßungspapagei, 1kg
(define parrot1 (make-parrot "Hello!" 1))
; Verabschiedungspapagei, 2kg
(define parrot2 (make-parrot "Goodbye!" 2))

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot1)
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
; - die leere Liste -ODER-
; - eine Cons-Liste bestehend aus erstem Element und Rest-Liste
;                                                         ^^^^^ Selbstbezug
(define list-of
  (lambda (element)
    (signature (mixed empty-list
                      (cons-list-of element)))))

; die leere Liste
(define-record empty-list
  make-empty
  empty?)
(define empty (make-empty))
#;(define-singleton empty-list ; Signatur
  empty ; Wert
  empty?)

; Eine Cons-Liste besteht aus:
; - erstes Element -UND-
; - Rest-Liste
(define-record (cons-list-of element) ; sorgt dafür, daß define-record ein lambda macht
  cons
  cons?
  (first element)
  (rest (list-of element)))

; 1elementige Liste: 5
(define list1 (cons 5 empty))
; 2elementige Liste: 5 8
(define list2 (cons 5 (cons 8 empty)))
; 3elementige Liste: 2 5 8
(define list3 (cons 2 (cons 5 (cons 8 empty))))
; 4elementige Liste: 7 2 5 8
(define list4 (cons 7 list3))

(define list-of-numbers (signature (list-of number)))

; Elemente einer Liste aufsummieren
(: list-sum ((list-of number) -> number))

(check-expect (list-sum list4)
              22)

; Schablone
#;(define list-sum
  (lambda (list)
    (cond
      ((empty? list) ...)
      ((cons? list)
       ... (first list) ...
       ... (list-sum (rest list)) ...))))

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0)
      ((cons? list)
       (+ (first list) 
          (list-sum (rest list)))))))

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
           (cons (first list) (extract-odds (rest list)))
           (extract-odds (rest list)))))))

; abstrahieren:
; - kopieren
; - ggf. umbenennen (rekursive Aufrufe nicht vergessen!)
; - Unterschiede -> abstrakte Namen
; - abstrakte Namen -> Parameter in einem lambda
;   (rekursive Aufrufe nicht vergessen!)

; Elemente einer Liste extrahieren, die ein bestimmtes Kriterium erfüllen
; Higher-Order-Funktion
;(: extract ((number -> boolean) list-of-numbers -> list-of-numbers))
; %element: Signaturvariable
; parametrische Polymorphie
(: extract ((%element -> boolean) (list-of %element) -> (list-of %element)))

(check-expect (extract odd? list4)
              (cons 7 (cons 5 empty)))
              
(define extract
  (lambda (p? list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (p? (first list))
           (cons (first list) (extract p? (rest list)))
           (extract p? (rest list)))))))

(define highway (cons dillo1 (cons dillo2 empty)))

; Liste von Tieren überfahren

; Liste von Zahlen inkrementieren
(: inc-list ((list-of number) -> (list-of number)))

(check-expect (inc-list list4)
              (cons 8 (cons 3 (cons 6 (cons 9 empty)))))

(define inc (lambda (n) (+ 1 n)))

(define inc-list
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons (inc (first list))
             (inc-list (rest list)))))))