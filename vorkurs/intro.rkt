;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname intro) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Ein Haustier ist eins der folgenden:
; - Hund
; - Katze
; - Schlange
; ^^^ allg. Fallunterscheidung
; speziell: Aufzählung
(define pet
  (signature (enum "Hund" "Katze" "Schlange")))

; Ist ein Haustier niedlich?
(: cute? (pet -> boolean))

(check-expect (cute? "Hund") #t)
(check-expect (cute? "Katze") #t)
(check-expect (cute? "Schlange") #f)

; Gerüust
#;(define cute?
  (lambda (pet)
    ...))

; Schablone
#;(define cute?
  (lambda (pet)
    ; Verzweigung
    (cond
      (... ...)
      (... ...)
      (... ...)
      )))

(define cute?
  (lambda (pet)
    ; Verzweigung
    (cond
      ((string=? pet "Hund") #t)
      ((string=? pet "Katze") #t)
      ((string=? pet "Schlange") #f)
      )))

; (cute? "Spinne")

; Ein Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot
; - Gewicht

; "besteht aus"
; zusammengesetzte Daten
(define-record dillo ; Signatur
  make-dillo ; Konstruktor
  dillo? ; Prädikat
  (dillo-alive? boolean) ; Selektor
  (dillo-weight rational))

(: dillo? (any -> boolean))

(define dillo1 (make-dillo #t 12)) ; Gürteltier, lebendig, 12kg
(define dillo2 (make-dillo #f 10)) ; dito, tot, 10kg

(define default-dillo (make-dillo #t 6))

; Funktionales Update
(: dillo-update-weight (dillo rational -> dillo))

(check-expect (dillo-update-weight dillo1 14)
              (make-dillo #t 14))

(define dillo-update-weight
  (lambda (dillo weight)
    (make-dillo (dillo-alive? dillo)
                weight)))

(: make-dillo (boolean number -> dillo))
(: dillo-alive? (dillo -> boolean))
(: dillo-weight (dillo -> number))

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

; QuickCheck
(check-property
 (for-all ((d dillo))
   (expect (run-over-dillo d)
           (make-dillo #f (dillo-weight d)))))

#;(check-expect (run-over-dillo dillo1)
              (make-dillo #f (dillo-weight dillo1)))
#;(check-expect (run-over-dillo dillo2)
              (make-dillo #f (dillo-weight dillo2)))
#;(check-expect (dillo-alive? (run-over-dillo dillo1))
              #f)
#;(check-expect (run-over-dillo dillo2)
              dillo2)

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))

; Eine Klapperschlange hat folgende Eigenschaften:
; - Länge
; - Dicke
(define-record snake
  make-snake
  snake?
  (snake-length rational)
  (snake-thickness rational))

(define snake1 (make-snake 300 5)) ; Schlange, 3m lang, 5cm dick
(define snake2 (make-snake 500 10)) ; Schlange, 5m lang, 10cm dick

(define run-over-snake
  (lambda (snake)
    (make-snake (snake-length snake)
                0)))

; Ein Tier (auf dem texanischen Highway) ist eins der folgenden:
; - Gürteltier
; - Klapperschlange
; Fallunterscheidung
; gemischte Daten (Summe, discriminated union)
(define animal
  (signature (mixed dillo snake)))

; Tier überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal dillo1)
              (run-over-dillo dillo1))
(check-expect (run-over-animal snake1)
              (run-over-snake snake1))

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (run-over-dillo animal))
      ((snake? animal) (run-over-snake animal)))))
              
; Eine Liste - erstmal aus Zahlen - ist eins der folgenden:
; - die leere Liste
; - eine Cons-Liste bestehend aus
;   einem ersten Element und einer Rest-Liste
;                                       ^^^^^ Selbstbezug
(define list-of-numbers
  (signature (mixed empty-list cons-list)))

; Die leere Liste ... es gibt nur eine.
(define-record empty-list
  make-empty-list
  empty?)

(define empty (make-empty-list))

; Eine Cons-Liste besteht aus:
; - erstes Element
; - Rest
(define-record cons-list
  cons
  cons?
  (first number)
  (rest list-of-numbers)) ; Selbstreferenz

; 1elementige Liste: 5
(define list1 (cons 5 empty))
; 2elementige Liste: 7 5
(define list2 (cons 7 (cons 5 empty)))
; 3elementige Liste: 12 7 5
(define list3 (cons 12 list2))

; Liste von Zahlen aufsummieren
(: list-sum (list-of-numbers -> number))

(check-expect (list-sum list2)
              12)
(check-expect (list-sum list3)
              24)

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


; Alle positiven Zahlen aus einer Liste extrahieren
(: extract-positive (list-of-numbers -> list-of-numbers))

(check-expect (extract-positive
               (cons -1
                     (cons 2
                           (cons -5
                                 (cons 4
                                       (cons 7 empty))))))
              (cons 2 (cons 4 (cons 7 empty))))

(define extract-positive
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (define rest-positives (extract-positive (rest list)))
       (if (positive? (first list))
           (cons (first list)
                 rest-positives)
           rest-positives)))))


(check-expect (extract
               positive?
               (cons -1
                     (cons 2
                           (cons -5
                                 (cons 4
                                       (cons 7 empty))))))
              (cons 2 (cons 4 (cons 7 empty))))

(define extract
  (lambda (p? list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (define rest-p (extract p? (rest list)))
       (if (p? (first list))
           (cons (first list)
                 rest-p)
           rest-p)))))