#lang deinprogramm/sdp/beginner
; data definition
; Pet is one of the following:
; - dog     OR
; - cat     OR
; - snake
; enumeration / case analysis
(define pet
  (signature (enum "dog" "cat" "snake")))