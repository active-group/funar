#lang deinprogramm/sdp/beginner

; Konstruktionsanleitungen / design recipes

; Datenanalyse

; Datendefinition

; Ein Haustier ist eins der folgenden:
; - Hund - ODER -
; - Katze - ODER -
; - Schlange
; Fallunterscheidung
; hier speziell: Aufzählung

; Datendefinition -> Signatur
(define pet
  (signature (enum "dog" "cat" "snake")))

; Ist ein Haustier niedlich?