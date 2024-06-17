#lang deinprogramm/sdp

(define shower-product
  (signature (mixed soap shampoo showergel)))

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

; - Duschgel - bestehen aus zwei Duschprodukten
(define-record showergel
  make-showergel
  showergel?
  (showergel-product1 shower-product)
  (showergel-product2 shower-product))
