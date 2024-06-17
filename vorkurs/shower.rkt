#lang deinprogramm/sdp

; Ein Duschprodukt ist eins der folgenden:
; - Seife -ODER-
; - Shampoo -ODER-
; - Duschgel
(define shower-product
  (signature (mixed soap shampoo showergel)))

; - Seife hat folgende Eigenschaft: pH-Wert
(define-record soap
  make-soap
  soap?
  (soap-ph number))

(define soap1 (make-soap 7))
(define soap2 (make-soap 9))

; - Shampoo hat folgende Eigenschaft: Haartyp
(define hairtype
  (signature (enum "normal" "oily" "dandruff" "none")))

(define-record shampoo
  make-shampoo
  shampoo?
  (shampoo-hairtype hairtype))

(define shampoo1 (make-shampoo "normal"))
(define shampoo2 (make-shampoo "oily"))

; - Duschgel - bestehen aus zwei Duschprodukten
(define-record showergel
  make-showergel
  showergel?
  (showergel-product1 shower-product) ; Selbstbezug
  (showergel-product2 shower-product))

(: make-showergel (shower-product shower-product -> shower-product))

(define gel1 (make-showergel soap1 shampoo1))
(define gel2 (make-showergel gel1 soap2))

; Seifenanteil eines Duschprodukts ermitteln
(: soap-proportion (shower-product -> rational))

(check-expect (soap-proportion soap1)
              1)
(check-expect (soap-proportion shampoo1)
              0)
(check-expect (soap-proportion gel1)
              0.5)
(check-expect (soap-proportion gel2)
              0.75)

(define soap-proportion
  (lambda (shower-product)
    (cond
      ((soap? shower-product) 1)
      ((shampoo? shower-product) 0)
      ((showergel? shower-product)
       ))))
