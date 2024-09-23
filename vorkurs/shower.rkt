#lang deinprogramm/sdp/beginner
; Duschprodukte

; Duschprodukt ist eins der folgenden:
; - Seife ... pH-Wert -ODER-
; - Shampoo ... Haartyp -ODER-
; - Duschgel, bestehend aus gleichen Teilen Seife und Shampoo
; Fallunterscheidung
(define shower-product
  (signature (mixed soap shampoo shower-gel)))

; Duschgel besteht aus:
; - Seife -UND-
; - Shampoo
(define-record shower-gel
  make-shower-gel
  shower-gel?
  (shower-gel-soap soap)
  (shower-gel-shampoo shampoo))

; Seife hat folgende Eigenschaft:
; - pH-Wert
; zusammengesetzte Daten
(define-record soap
  make-soap
  soap?
  (soap-ph real))

; Shampoo hat folgende Eigenschaft:
; - Haartyp
; zusammengesetzte Daten
(define-record shampoo
  make-shampoo
  shampoo?
  (shampoo-hair-type hair-type))

; Haartyp:
; - Schuppen
; - Trocken
; - Fettig
(define hair-type
  (signature (enum "dandruff" "dry" "oily")))
