#lang deinprogramm/sdp/beginner

; Datendefinition:
; Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; Fallunterscheidung
; hier speziell: Aufzählung / "Enumeration"

(define pet
  (signature
   (enum "dog"
         "cat"
         "snake")))

; Ist Haustier niedlich?
(: cute? (pet -> boolean))

(check-expect (cute? "dog")
              #t)
(check-expect (cute? "cat")
              #t)
(check-expect (cute? "snake")
              #f)

; Gerüst
#;(define cute?
  (lambda (pet)
    ...))

; hängt nur von Signatur ab:
; Schablone
#;(define cute?
  (lambda (pet)
    (cond ; Verzweigung
      ; ein Zweig pro Fall
      ; Format: (<Bedingung> <Ergebnis>)
      ((equal? pet "dog") ...)
      ((equal? pet "cat") ...)
      ((equal? pet "snake") ...))))

(define cute?
  (lambda (pet)
    (cond ; Verzweigung
      ; ein Zweig pro Fall
      ; Format: (<Bedingung> <Ergebnis>)
      ((equal? pet "dog") #t)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f))))

; Uhrzeit besteht aus / hat folgende Eigenschaften:
; - Stunde -UND-
; - Minute
; zusammengesetzte Daten
(define-record time
  make-time ; Konstruktor
  ; Felder:
  (time-hour   (integer-from-to 0 23)) ; Selektor, "Getter-Funktion"
  (time-minute (integer-from-to 0 59)))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; natural: natürliche Zahlen, 0,1,2,3,4,5,6,...

; 11 Uhr 27 Minuten
(define time1 (make-time 11 27))
; 14:13
(define time2 (make-time 14 13))

; Minuten seit Mitternacht
(: msm (time -> natural))

(check-expect (msm time1)
              687)
(check-expect (msm time2)
              853)

; Schablone
#;(define msm
  (lambda (time)
    ... (time-hour time) ...
    ... (time-minute time) ...))

; NICHT: hour time-hour (time-hour)

(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

; Aus Minuten seit Mitternacht das entsprechende time-Objekt konstruieren
(: msm->time (natural -> time))

; Schablone
#;(define msm->time
  (lambda (minutes)
    (make-time ... ...)))

(check-expect
 (msm->time (msm time1))
 time1)

(check-expect
 (msm->time (msm time2))
 time2)

; Property-based testing / QuickCheck
(check-property
 (for-all ((t time))
   (equal?
    (msm->time (msm t))
    t)))

(define msm->time
  (lambda (minutes)
    (make-time (quotient minutes 60)
               (remainder minutes 60))))

; Tier auf dem texanischen Highway ist eins der folgenden:
; - Gürteltier -ODER-
; - Papagei
; Fallunterscheidung: Fälle haben selber Datendefinitionen
; -> gemischte Daten
(define animal
  (signature (mixed dillo parrot)))

; Gürteltier hat folgende Eigenschaften:
; - lebendig? -UND-
; - Gewicht
(define-record dillo
  make-dillo ; Konstruktor
  dillo? ; Prädikat
  (dillo-alive? boolean)
  (dillo-weight number))
; "Zustand des Gürteltiers zu einem bestimmten Zeitpunkt"

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
    ...
    (dillo-alive? dillo)
    (dillo-weight dillo)
    ...))

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))

; Gürteltier füttern um wählbare Menge
; (Tote Gürteltiere fressen nicht.)

(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 2)
              (make-dillo #t 12))
(check-expect (feed-dillo dillo2 2)
              dillo2)

(define feed-dillo
  (lambda (dillo amount)
    (define alive? (dillo-alive? dillo))
    (define weight (dillo-weight dillo))
    (make-dillo
     alive?
     (if alive? ; syntaktischer Zucker
         (+ weight amount)
         weight)
     #;(cond
       ((dillo-alive? dillo)
        (+ (dillo-weight dillo) amount))
       (else   ; (not (dillo-alive? dillo))
        (dillo-weight dillo))))))

; Papagei hat folgende Eigenschaften:
; - Satz -UND-
; - Gewicht
(define-record parrot
  make-parrot
  parrot?
  (parrot-sentence string)
  (parrot-weight number))

; Begrüßungspapagei
(define parrot1 (make-parrot "Hallo!" 1))
(define parrot2 (make-parrot "Tschüss!" 2))

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
              (make-dillo #f 10))
(check-expect (run-over-animal parrot1)
              (run-over-parrot parrot1))

; Schablone
#;(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) ...)
      ((parrot? animal) ...))))

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (run-over-dillo animal))
      ((parrot? animal) (run-over-parrot animal)))))

; Liste ist eins der folgenden:
; - die leere Liste
; - eine Cons-Liste, bestehend aus erstem Element und Rest-Liste
;                                                          ^^^^^ Selbstbezug

(define list-of-numbers
  (signature (mixed empty-list
                    cons-list)))

; die leere Liste ist ... ES GIBT NUR EINE
(define-singleton empty-list ; Signatur
  empty ; Singleton-Wert
  empty?) ; Prädikat

; Eine Cons-Liste besteht aus:
; - erstes Element  -UND-
; - Rest-Liste
(define-record cons-list
  cons
  cons?
  (first number) ; vorläufig
  (rest list-of-numbers))


; 1elementige Liste: 5
(define list1 (cons 5 empty))

; 2elementige Liste: 2 5
(define list2 (cons 2 (cons 5 empty)))

; 3elementige Liste: 2 5 8
(define list3         (cons 2 (cons 5 (cons 8 empty))))

; 4elementige Liste: 3 2 5 8
;(define list4 (cons 3 (cons 2 (cons 5 (cons 8 empty)))))
(define list4 (cons 3 list3))