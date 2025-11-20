#lang deinprogramm/sdp
; Eine Linse besteht aus:
; - Getter
; - "functional update"
(define-record lens
  make-lens
  lens?
  (lens-get (%a -> %b))
  (lens-update (%a %b -> %a)))

(define-record dillo
  make-dillo
  dillo? ; Prädikat
  (dillo-alive? boolean)
  (dillo-weight number))

(define dillo1 (make-dillo #t 10))

(define dillo-alive?*
  (make-lens dillo-alive?
             (lambda (dillo alive?)
               (make-dillo alive?
                           (dillo-weight dillo)))))
(define dillo-weight*
  (make-lens dillo-weight
             (lambda (dillo weight)
               (make-dillo (dillo-alive? dillo)
                           weight))))

; Feld mit Linse extrahieren
(define yank
  (lambda (object lens)
    ((lens-get lens) object)))

(define shove
  (lambda (object lens new)
    ((lens-update lens) object new)))

(define compose
  (lambda (lens1 lens2)
    (make-lens (lambda (object)
                 (yank (yank object lens1) lens2))
               (lambda (object new)
                 (shove object lens1
                        (shove (yank object lens1)
                               lens2
                               new))))))

