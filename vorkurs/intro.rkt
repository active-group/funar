;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname intro) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))


(: x number)
(define x (+ 12 23))
(define y
  (* 12
     (* 23
        42)))

(define f
  (lambda (x)
    (+ x y)))

; Bindung zu einem Vorkommen einer Variable:
; - innen nach außen suchen
; - lambda mit Parameter
; - Definition
; - eingebaute / importierte Definition
; lexikalische / statische Bindung
(: circle1 image)
(define circle1 (circle 50 "solid" "red"))
(define square1 (square 100 "outline" "green"))
(define star1 (star 50 "solid" "green"))
(define overlay1 (overlay star1 circle1))

; Zeilenkommentar

#;(above
 (beside circle1 star1)
 (beside star1 circle1))

#;(above
 (beside overlay1 circle1)
 (beside circle1 overlay1))

; Konstruktionsanleitung

; Kurzbeschreibung
; Quadratisches Kachelmuster erzeugen

; Signaturdeklaration
(: tile (image image -> image))

; Tests
(check-expect (tile circle1 star1)
              (above
               (beside circle1 star1)
               (beside star1 circle1)))

; Funktionsdefinition

; Gerüst:
#;(define tile
  (lambda (image1 image2)
    ...))

(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))

; (tile circle1 star1)
;(tile "circle1" "star1")

#|
class C {
  static int f(int x) {
    x = x * 2;
    return x + 1;
  }

  ... f(42) ...
}

|#

; Datenanalyse

; Datendefinition

; Haustier ist eins der folgenden:
; - Hund - ODER -
; - Katze - ODER -
; - Schlange
; Fallunterscheidung
; speziell: Aufzählung
(define pet (signature (enum "dog" "cat" "snake")))

; Ist Haustier niedlich?
(: cute? (pet -> boolean))

(check-expect (cute? "cat") #t)
(check-expect (cute? "dog") #t)
(check-expect (cute? "snake") #f)

; Gerüst
#;(define cute?
  (lambda (pet)
    ...))

; Schablone
; wird aus der Signatur hergeleitet
#;(define cute?
  (lambda (pet)
    ; Verzweigung, 1 Zweig pro Fall
    ; Jeder Zweig besteht aus: (Bedingung Antwort)
    (cond
      ((string=? pet "cat") ...)
      ((string=? pet "dog") ...)
      ((string=? pet "snake") ...))))

(define cute?
  (lambda (pet)
    ; Verzweigung, 1 Zweig pro Fall
    ; Jeder Zweig besteht aus: (Bedingung Antwort)
    (cond
      ((string=? pet "cat") #t)
      ((string=? pet "dog") #t)
      ((string=? pet "snake") #f))))

; Uhrzeit besteht aus / hat folgende Eigenschaften:
; - Stunde - UND -
; - Minute
; zusammengesetzte Daten
(define-record time
  make-time ; Konstruktor
  (time-hour natural) ; Selektor
  (time-minute natural))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

; 12 Uhr 24 Minuten
(define time1 (make-time 12 24))
; 16:12 Uhr
(define time2 (make-time 16 12))

; Minuten seit Mitternacht ermitteln
(: msm (time -> natural))

(check-expect (msm time1)
              744)
(check-expect (msm time2)
              972)

; Schablone
#;(define msm
  (lambda (time)
    ... (time-hour time) ... (time-minute time) ...))

(define msm
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))