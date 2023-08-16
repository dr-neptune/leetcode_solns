#lang racket
(require racket)

(define (binet n)
  (/ (- (expt (+ 1 (sqrt 5)) n)
        (expt (- 1 (sqrt 5)) n))
     (* (expt 2 n) (sqrt 5))))

(define (fib n) (exact-round (binet n)))

(fib 100)

(define (binet n)
  (let* ((sqrt-5 (exact-integer-sqrt 5))
         (numerator (exact-integer-pow (+ 1 sqrt-5) n))
         (denominator (exact-integer-pow (- 1 sqrt-5) n)))
    (exact-integer-div (- numerator denominator)
                      (* (exact-integer-pow 2 n) sqrt-5))))

flonum

(scheme_make_bignum 5)


;; use generator from racket/math library
(define (generator a b p q count)
  (cond [(zero? count) b]
        [(even? count)
         (generator a b
                    (+ (* p p) (* q q))
                    (+ (* 2 p q) (* q q))
                    (quotient count 2))]
        [else
         (generator (+ (* b q) (* a q) (* a p))
                    (+ (* b p) (* a q))
                    p q (sub1 count))]))


(define ((make-fibonacci a b) n)
  (generator b a 0 1 n))

(define (fib n)
  (define fib-gen (make-fibonacci 0 1))
  (match n
    [(? negative?)
     (let ([base-fib (fib-gen (abs n))])
       (if (even? n)
           (- base-fib)
           base-fib))]
    [_ (fib-gen n)]))
