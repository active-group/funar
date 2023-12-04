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
