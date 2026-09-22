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
(define-record time ; signature
  make-time ; constructor
  (time-hour natural) ; selector / signature: natural number
  (time-minute natural))

; 11:44
(define time1 (make-time 11 44))
; 15:12
(define time2 (make-time 15 12))

; minutes since midnight
(: minutes-since-midnight (time -> natural))

(check-expect (minutes-since-midnight time1)
              704)
(check-expect (minutes-since-midnight time2)
              912)

(define minutes-since-midnight
  (lambda (time)
    (+ (* 60 (time-hour time))
       (time-minute time))))

; compute time from the minutes since midnight

