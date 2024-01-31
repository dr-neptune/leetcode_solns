#lang racket
(require racket)


(define moving-average%
  (class object%
    (super-new)

    ; size : exact-integer?
    (init-field
     size
     [vals '()]
     [curr-size 1])

    ; next : exact-integer? -> flonum?
    (define/public (next val)
      (set! vals (cons val vals))
      (if (not (equal? size curr-size))
          (begin
            (set! curr-size (add1 curr-size))
            (/ (apply + (take vals (sub1 curr-size))) (sub1 curr-size)))
          (/ (apply + (take vals size)) size)))))
