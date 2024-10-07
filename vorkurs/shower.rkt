#lang deinprogramm/sdp/beginner

; Duschprodukt ist eins der folgenden:
; - Seife -ODER-
; - Shampoo -ODER-
; - Duschgel: besteht aus gleichen Teilen aus Seife und Shampoo
; gemischte Daten
(define shower-product
  (signature (mixed soap
                    shampoo
                    showergel)))

; Seife hat folgende Eigenschaften:
; - pH-Wert
(define-record soap
  make-soap
  soap?
  (soap-ph number))

(define soap1 (make-soap 7.0))
(define soap2 (make-soap 8.0))

; Haartyp:
; - Schuppen -ODER-
; - trocken -ODER-
; - fettig
(define hairtype
  (signature (enum "dandruff" "dry" "oily")))

; Shampoo hat folgende Eigenschaften:
; - Haartyp
(define-record shampoo
  make-shampoo
  shampoo?
  (shampoo-hairtype hairtype))

(define shampoo1 (make-shampoo "dandruff"))
(define shampoo2 (make-shampoo "dry"))

; Duschgel besteht aus:
; - Seife
; - Shampoo
(define-record showergel
  make-showergel
  showergel?
  (showergel-soap soap)
  (showergel-shampoo shampoo))

(define gel1 (make-showergel soap1 shampoo1))
(define gel2 (make-showergel soap2 shampoo2))

; Mixtur aus Duschgel und Seife
(define-record mixture-a
  make-mixture-a
  mixture-a?
  (mixture-a-showergel showergel)
  (mixture-a-soap soap))

(define-record mixture-z
  make-mixture-z
  mixture-z?
  (mixture-a-showergel1 showergel)
  (mixture-a-showergel2 showergel))

; Eine Mixture besteht aus:
; - Duschprodukt
; - noch'n Duschprodukt
(define-record mixture
  make-mixture
  mixture?
  (mixture-product1 shower-product)
  (mixture-product2 shower-product))

(define mix1 (make-mixture gel1 soap2))
(define mix2 (make-mixture gel1 gel2))