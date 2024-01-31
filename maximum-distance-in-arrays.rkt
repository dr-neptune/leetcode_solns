#lang racket
(require racket)

(define exlol '((1 2 3) (4 5) (1 2 3)))
(define exlol '((1) (1)))
(define exlol '((1) (2)))
(define exlol '((1 4) (0 5)))
(define exlol '((-1 5 11) (6 10)))


(let ([arrays exlol])
  (for/fold ([mx 0]
             [max-val -Inf.0]
             [min-val +Inf.0]
             #:result (inexact->exact mx))
            ([arr arrays])
    (values (max mx (max (- max-val (first arr)) (- (last arr) min-val)))
            (max max-val (last arr))
            (min min-val (first arr)))))


(define (max-distance arrays)
  (for/fold ([max-distance 0]
             [max-val -Inf.0]
             [min-val +Inf.0]
             #:result (inexact->exact max-distance))
            ([arr arrays])
    (values (max max-distance
                 (max (- max-val (first arr))
                      (- (last arr) min-val)))
            (max max-val (last arr))
            (min min-val (first arr)))))
