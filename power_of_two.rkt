#lang racket
(require racket)
(require math/bigfloat)

;; without loops
(define (num-digits n)
  (if (= n 0) 1 (+ 1 (floor (log n 10)))))

(define (bfdampen n [tol 0.01])
  (bf/ (bf tol)
       (bfexp10 (bf (num-digits n)))))

(define (is-power-of-two n)
  (let ([n (abs n)]
        [val (bflog2 (bf n))])
    (bf< (bfabs (bf- val (bfround val)))
         (bfdampen n))))

;; with loops
(define (is-power-of-two-loops n)
  (cond [(not (exact-integer? n)) #f]
        [(= n 1) #t]
        [(< n 1) #f]
        [else (is-power-of-two-loops (/ n 2))]))

;; tests
(module+ test
  (require rackunit)
  (check-true (is-power-of-two 536870912))
  (check-false (is-power-of-two 511))
  (check-false (is-power-of-two 255))
  (check-false (is-power-of-two 0))
  (check-false (is-power-of-two -16)))

(module+ test
  (require rackunit)
  (check-true (is-power-of-two-loops 536870912))
  (check-false (is-power-of-two-loops 511))
  (check-false (is-power-of-two-loops 255))
  (check-false (is-power-of-two-loops 0))
  (check-false (is-power-of-two-loops -16)))
