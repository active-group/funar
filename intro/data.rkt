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
  (time-hour (integer-from-to 0 23)) ; selector / signature: natural number
  (time-minute (integer-from-to 0 59)))

(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))

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

(: msm->time (natural -> time))

(check-expect (msm->time 704)
              time1)
(check-expect (msm->time 912)
              time2)

(define msm->time
  (lambda (minutes)
    (make-time (remainder (quotient minutes 60) 24)
               (remainder minutes 60))))
                             

; Animals on the Texas highway

; Armadillo has the following attributes:
; - alive OR dead?     AND
; - weight
(define-record dillo
  make-dillo
  (dillo-alive? boolean)
  (dillo-weight number))

(: make-dillo (boolean number -> dillo))

; live armadillo, 10kg
(define dillo1 (make-dillo #t 10))
; dead armadillo, 8kg
(define dillo2 (make-dillo #f 8))

; run over an armadillo
(: run-over-dillo (dillo -> dillo))

(check-expect (run-over-dillo dillo1)
              (make-dillo #f 10))
(check-expect (run-over-dillo dillo2)
              dillo2)

(define weight 15)

(define run-over-dillo
  (lambda (dillo)
    (define weight (dillo-weight dillo))
    (make-dillo #f
                weight)))

; static lexical scope:
; from the use of a name, search outwards
; the first define or lambda is the corresponding binding


