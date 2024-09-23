#lang deinprogramm/sdp/beginner
; Duschprodukte

; Duschprodukt ist eins der folgenden:
; - Seife ... pH-Wert -ODER-
; - Shampoo ... Haartyp -ODER-
; - Mixtur aus zwei Duschprodukten
;                   ^^^^^^^^^^^^ Selbstbezug
; x Duschgel, bestehend aus gleichen Teilen Seife und Shampoo
; Fallunterscheidung
(define shower-product
  (signature (mixed soap
                    shampoo
                    mixture
                    #;shower-gel)))

; Eine Mixtur besteht aus:
; - Duschprodukt
; - nochn Duschprodukt
(define-record mixture
  make-mixture
  mixture?
  (mixture-product1 shower-product)
  (mixture-product2 shower-product))

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

(define sh1 (make-shampoo "dandruff"))
(define sh2 (make-shampoo "dry"))
(define sp1 (make-soap 8))
(define sp2 (make-soap 7))

(define gel1 (make-shower-gel sp1 sh1))
(define gel2 (make-shower-gel sp2 sh2))

(define mix1 (make-mixture sp1 sh1))
(define mix2 (make-mixture sp2 sh2))

(define mix3 (make-mixture mix1 sp2))
(define mix4 (make-mixture mix2 sh2))

; pH-Wert eines Duschprodukts ermitteln
(: shower-product-ph (shower-product -> real))

(check-expect (shower-product-ph sp1) 8)
(check-expect (shower-product-ph sh1) 7)
(check-expect (shower-product-ph mix1)
              7.5)

(define shower-product-ph
  (lambda (product)
    (match product
      ((make-soap ph)
       ph)
      ((make-shampoo type)
       7)
      ((make-mixture product1 product2)
       (/ (+ (shower-product-ph product1)
             (shower-product-ph product2))
          2)))))