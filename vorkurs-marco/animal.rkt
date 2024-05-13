#lang deinprogramm/sdp/beginner

; hunde, katzen, schlangen

; Datendefinition
; Ein Haustier ist eines der folgenden
; - Hund - ODER -
; - Katze - ODER -
; - Schlange
; Fallunterscheidung
; speziell hier: Aufzählung
(define pet
  (signature (enum "dog" "cat" "snake")))

; Konstruktionsanleitung
; 1. Kurzbeschreibung schreiben
; 2. Signatur
; 3. Test

; Ist ein Haustier niedlich?
(: cute? (pet -> boolean))

