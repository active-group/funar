#lang deinprogramm/sdp

; - Seife hat folgende Eigenschaft: pH-Wert
(define-record soap
  make-soap
  soap?
  (soap-ph number))

; - Shampoo hat folgende Eigenschaft: Haartyp
(define hairtype
  (signature (enum "normal" "oily" "dandruff" "none")))

(define-record shampoo
  make-shampoo
  shampoo?
  (shampoo-hairtype hairtype))

; - Duschgel - bestehend aus 50% Seife, 50% Shampoo
(define-record showergel
  make-showergel
  showergel?
  (showergel-soap soap)
  (showergel-shampoo shampoo))
