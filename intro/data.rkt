#lang deinprogramm/sdp/beginner
; data definition
; Pet is one of the following:
; - dog     OR
; - cat     OR
; - snake
; enumeration / case analysis
(define pet
  (signature (enum "dog" "cat" "snake")))

; is a pet cute?
(: cute? (pet -> boolean))

(check-expect (cute? "dog") #t)
(check-expect (cute? "cat") #t)
(check-expect (cute? "snake") #f)

; template
(define cute?
  (lambda (pet)
    (cond
      ; 1 branch per case (<condition> <result>)
      ((equal? pet "dog") #t)
      ((equal? pet "cat") #t)
      ((equal? pet "snake") #f))))

; Time consists of / has the following attributes:
; - hour     AND
; - minute
; compound data
(define-record time
  make-time
  (time-hour natural)
  (time-hour natural))
