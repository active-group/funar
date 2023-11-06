#lang deinprogramm/sdp/beginner

; Ist ein Haustier niedlich?

; Datendefinition:

; Ein Haustier ist eins der folgenden:
; - Hund -ODER-
; - Katze -ODER-
; - Schlange
; Fallunterscheidung
; hier speziell: Aufzählung
(define pet
  (signature (enum "dog" "cat" "snake")))

