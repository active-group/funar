;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname intro) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Datenanalyse
; Datendefinition:
; stilisierte, natürlichsprachliche Beschreibung der Daten

; Ein Haustier ist eins der folgenden:
; - Hund
; - Katze
; - Schlange
; Fallunterscheidung
; speziell: Aufzählung
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
    ; Verzweigung
    (cond
      ((string=? pet "dog") ...)
      ((string=? pet "cat") ...)
      ((string=? pet "snake") ...))))

(define cute?
  (lambda (pet)
    ; Verzweigung
    (cond
      ((string=? pet "dog") #t)
      ((string=? pet "cat") #t)
      ((string=? pet "snake") #f))))

; Ein Gürteltier hat folgende Eigenschaften: ("besteht aus")
; - lebendig oder tot
; - Gewicht
; zusammengesetzte Daten
(define-record dillo
  make-dillo ; Konstruktor
  dillo? ; Prädikat
  (dillo-alive? boolean) ; Selektor
  (dillo-weight number))

(: make-dillo (boolean number -> dillo))
(: dillo-alive? (dillo -> boolean))
(: dillo-weight (dillo -> number))
(: dillo? (any -> boolean))

; Gürteltier, lebendig, 10kg
(define dillo1 (make-dillo #t 10))
(define dillo2 (make-dillo #f 9)) ; totes Gürteltier, 9kg


; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))
(check-expect (run-over-dillo dillo2)
              dillo2)

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-weight dillo))))


; Gürteltier füttern
(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 3)
              (make-dillo #t 13))
(check-expect (feed-dillo dillo2 5)
              dillo2)

; obj1 == obj2

; Massenerhaltungssatz

(check-property
 (for-all ((d dillo)
           (amount number))
   (==> (>= amount 0)
        (>= (dillo-weight (feed-dillo d amount))
            (dillo-weight d)))))

(define feed-dillo
  (lambda (dillo amount)
    (make-dillo (dillo-alive? dillo)
                (if (dillo-alive? dillo)
                    (+ (dillo-weight dillo) amount)
                    (dillo-weight dillo)))))


#|
class Dillo {
  bool alive;
  void runOver() {
     this.alive = false;
  }
}
|#

; Ein Papagei hat folgende Eigenschaften:
; - Satz
; - Gewicht
(define-record parrot
  make-parrot
  parrot?
  (parrot-sentence string)
  (parrot-weight number))

; Papagei mit Gruß, 1kg 
(define parrot1 (make-parrot "Hello!" 1))
; sehr dicker Papagei, 2kg, verabschiedet sich
(define parrot2 (make-parrot "Tschüss!" 2))

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot1)
              (make-parrot "" 1))

(define run-over-parrot
  (lambda (parrot)
    (make-parrot "" (parrot-weight parrot))))

; Papagei füttern
(: feed-parrot (parrot number -> parrot))

(check-expect (feed-parrot parrot1 3)
              (make-parrot "Hello!" 4))

(define feed-parrot
  (lambda (parrot amount)
    (make-parrot (parrot-sentence parrot)
                 (+ (parrot-weight parrot) amount))))

; Ein Tier ist eins der folgenden:
; - Gürteltier
; - Papagei
; Fallunterscheidung
; hier speziell: gemischte Daten (aus unterschiedlichen Signaturen)
(define animal
  (signature (mixed dillo parrot)))

#|
interface Animal {
  void runOver();
  // Neue Funktion hinzufügen: schwer in OOP, einfach in FP
  void feed(double amount);
}
class Dillo implements Animal {
  void runOver() { ... }
}
class Parrot implements Animal {
  ...
}
// Neue Klasse hinzufügen: einfach in OOP, schwer in FP
class Sloth implements Animal {
  void runOver() { ... }
}
|#





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
     

; Java: List<E>

; Eine Liste ist eins der folgenden:
; - die leere Liste
; - eine Cons-Liste aus erstem Element und Rest-Liste
;                                               ^^^^^ Selbstbezug
; ^^^ gemischte Daten
(: list-of (signature -> signature))
(define list-of
  (lambda (element)
    (signature (mixed empty-list
                      (cons-list-of element)))))

(define-record empty-list
  make-empty
  empty?)

(define empty (make-empty))

; Eine Cons-Liste besteht aus:
; - erstes Element
; - Rest-Liste
(define-record (cons-list-of element) ; Klammern drum -> (lambda (element) ...)
  cons
  cons?
  (first element)
  (rest (list-of element)))

(define list-of-numbers (signature (list-of number)))

(define list1 (cons 5 empty)) ; 1elementige Liste: 5
(define list2 (cons 8 (cons 5 empty))) ; 2elementige Liste: 8 5
(define list3 (cons 13 list2)) ; 3elementige Liste: 13 8 5

; Summe der Listenelemente berechnen
(: list-sum (list-of-numbers -> number))

(check-expect (list-sum list3)
              26)

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0)
      ((cons? list)
       (+ (first list)
          (list-sum (rest list)))))))

#|
; Gerade Zahlen aus einer Liste extrahieren
(: extract-evens (list-of-numbers -> list-of-numbers))

(check-expect (extract-evens list3)
              (cons 8 empty))


#;(cond
  (... ...)
  (... ...)
  (else ...))

|#

; Positive Zahlen aus einer Liste extrahieren
(: extract-positives (list-of-numbers -> list-of-numbers))

(check-expect (extract-positives (cons -1 (cons 3 (cons -5 (cons 4 empty)))))
              (cons 3 (cons 4 empty)))

(define extract-positives
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (positive? (first list))
           (cons (first list)
                 (extract-positives (rest list)))
           (extract-positives (rest list)))))))

; Higher-Order-Funktion: mehrere Pfeile in der Signatur

; Listenelemente extrahieren, die ein Kriterium erfüllen
(: extract ((number -> boolean) list-of-numbers -> list-of-numbers))

(check-expect (extract even? (cons 2 (cons 3 (cons 4 (cons 5 empty)))))
              (cons 2 (cons 4 empty)))
(check-expect (extract positive? (cons -1 (cons 3 (cons -5 (cons 4 empty)))))
              (cons 3 (cons 4 empty)))

(define extract
  (lambda (p? list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (p? (first list))
           (cons (first list)
                 (extract p? (rest list)))
           (extract p? (rest list)))))))