#lang deinprogramm/sdp/beginner
; Datendefinition
; Haustier ist eins der folgenden:
; - Hund ODER
; - Katze ODER
; - Schlange
; Fallunterscheidung / Summe
; Aufzählung

; -> für jede Datendefinition eine Signatur
(define pet (signature (enum "dog" "cat" "snake")))