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
                             

; Animal on the Texas highway:
; - armadillo  OR
; - parrot
; mixed data
(define animal (signature (mixed dillo parrot)))

; Armadillo has the following attributes:
; - alive OR dead?     AND
; - weight
(define-record dillo
  make-dillo
  dillo? ; predicate
  (dillo-alive? boolean)
  (dillo-weight number))

(: make-dillo (boolean number -> dillo))
(: dillo? (any -> boolean))

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

; feed an armadillo, by a variable amount

(: feed-dillo (dillo number -> dillo))

(check-expect (feed-dillo dillo1 5)
              (make-dillo #t 15))
(check-expect (feed-dillo dillo2 5)
              dillo2)

#;(define feed-dillo
  (lambda (dillo amount)
    (cond
      ((dillo-alive? dillo)
       (make-dillo #t (+ (dillo-weight dillo) amount)))
      ((not (dillo-alive? dillo))
       dillo))))

#;(define feed-dillo
  (lambda (dillo amount)
    (if (dillo-alive? dillo)
        (make-dillo #t (+ (dillo-weight dillo) amount))
        dillo)
    #;(cond
      ((dillo-alive? dillo)
       (make-dillo #t (+ (dillo-weight dillo) amount)))
      (else
       dillo))))


(define feed-dillo
  (lambda (dillo amount)
    (define alive? (dillo-alive? dillo))
    (define weight (dillo-weight dillo))
    (make-dillo alive?
                (if alive?
                    (+ amount weight)
                    weight))))

; Parrot has the following attributes:
; - sentence  AND
; - weight
; compound data
(define-record parrot
  make-parrot
  parrot?
  (parrot-sentence string)
  (parrot-weight number))

; greetings parrot, 1kg
(define parrot1 (make-parrot "welcome!" 1))
(define parrot2 (make-parrot "Good riddance!" 2))

; run over a parrot
(: run-over-parrot (parrot -> parrot))

(check-expect (run-over-parrot parrot1)
              (make-parrot "" 1))

(define run-over-parrot
  (lambda (parrot)
    (make-parrot "" (parrot-weight parrot))))

; run over an animal
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal dillo1)
              (make-dillo #f 10))
(check-expect (run-over-animal parrot1)
              (make-parrot "" 1))

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (run-over-dillo animal))
      ((parrot? animal) (run-over-parrot animal)))))

; A list is one of the following:
; - the empty list  OR
; - a cons list consisting of the first element AND a rest list
;                                                          ^^^^ self-reference

(define list-of-numbers
  (signature (mixed empty-list cons-list)))

; start with lists of numbers
(define-singleton empty-list ; signature
  empty ; singleton
  empty?) ; predicate

(define-record cons-list
  cons
  cons?
  (first number)
  (rest list-of-numbers))