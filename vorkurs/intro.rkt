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
