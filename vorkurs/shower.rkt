#lang deinprogramm/sdp/beginner

; Duschprodukt ist eins der folgenden:
; - Seife -ODER-
; - Shampoo -ODER-
; - Duschgel: besteht aus gleichen Teilen aus Seife und Shampoo
; gemischte Daten

; Seife hat folgende Eigenschaften:
; - pH-Wert
(define-record soap
  make-soap
  soap?
  (soap-ph number))

; Shampoo hat folgende Eigenschaften:
; - Haartyp
(define-record shampoo
  make-shampoo
  shampoo?
  (shampoo-hairtype hairtype))

; Haartyp:
; - Schuppen -ODER-
; - trocken -ODER-
; - fettig
(define hairtype
  (signature (enum "dandruff" "dry" "oily")))
