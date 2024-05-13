#lang deinprogramm/sdp/beginner

; hunde, katzen, schlangen

; Datendefinition
; Ein Haustier ist eines der folgenden
; - Hund - ODER -
; - Katze - ODER -
; - Schlange - ODER -
; - Fruchtfliege
; Fallunterscheidung
; speziell hier: Aufzählung
(define pet
  (signature (enum "dog" "cat" "snake" "fruit fly")))

; Konstruktionsanleitung
; 1. Kurzbeschreibung schreiben
; 2. Signatur
; 3. Test
; 4. Gerüst

; Ist ein Haustier niedlich?
(: cute? (pet -> boolean))
#;(: cute? ((enum "dog" "cat" "snake" "fruit fly") -> boolean))

(check-expect (cute? "dog") #t)
(check-expect (cute? "cat") #t)
(check-expect (cute? "snake") #t)
(check-expect (cute? "fruit fly") #f)

(define cute?
  (lambda (animal)
    (cond
      ((string=? "dog" animal) #t)
      ((string=? "cat" animal) #t)
      ((string=? "snake" animal) #t)
      ((string=? "fruit fly" animal) #f))))

; Schablone für gemischte Daten/Falluntescheidung

#;(define XXX
    (lambda (l)
      (cond
        (fall1 konsequente1)
        (fall2 konsequente2))))