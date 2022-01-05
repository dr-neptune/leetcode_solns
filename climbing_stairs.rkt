#lang racket
(require racket)

;; with fibonacci recursion
(define-syntax define/memoized
  (syntax-rules ()
    [(_ (f args ...) bodies ...)
     (define f
       (let ([results (make-hash)])
         (lambda (args ...)
           ((lambda vals
              (when (not (hash-has-key? results vals))
                (hash-set! results vals (begin bodies ...)))
              (hash-ref results vals))
            args ...))))]))


(define/memoized (climb-stairs n)
  (cond [(= n 1) 1]
        [(= n 2) 2]
        [else (+ (climb-stairs (- n 1))
                 (climb-stairs (- n 2)))]))

(time (climb-stairs 2000))

;; with an approximation to Binet's Formula
;; https://en.wikipedia.org/wiki/Fibonacci_number#Computation_by_rounding
(define (climb-stairs n)
  (exact-round (/ (expt 1.618033988749895 (add1 n))
                  (sqrt 5))))

(time (climb-stairs 2000))
