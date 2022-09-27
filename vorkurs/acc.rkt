#lang deinprogramm/sdp

#;(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0)
      ((cons? list)
       (+
        (first list)
        (list-sum (rest list)))))))

(check-expect (list-sum* (list 1 2 3 4) 0)
              10)

  #|
  list = ...;
  acc = 0;
  while (list != empty) {
    acc = (first list) + acc;
    list = (rest list);
  }
  return acc;
  |#

(define list-sum
  (lambda (list0)
    (define list-sum*
      ; acc: Summe der "gesehenen" Listenelemente
      (lambda (list acc)
        (cond
          ((empty? list) acc)
          ((cons? list)
           (list-sum* (rest list)
                      ; neues Zwischenergebnis aus dem alten und (first list) berechnen
                      (+ (first list) acc))))))
    (list-sum* list0 0)))
    
       
    

;(list-sum (list 1 2 3 4))