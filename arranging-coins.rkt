#lang racket
(require racket)

;; analysis
;; we know that each stair step is 1 + 2 + 3 + ... = n(n+1)/2 coins to fill completely
;; then 5 coins can fill 1 + 2 = 3 + 3 = 6 == (5 + 1)
;; so basically we need to find the value n(n+1)/2 s.t. our input z is greater than (n-1)n/2 < z <= n(n+1)/2
;; then we will have the number of stairs (n-1)

(define (calc-arithmetic n)
  (/ (* n (add1 n)) 2))

(calc-arithmetic 2)
(calc-arithmetic 3)

;; naive approach: calculate these values until we hit one that is >= z, then return n if =, else return n - 1

(calc-arithmetic 2)

(define (calc-arithmetic n)
  (/ (* n (add1 n)) 2))

(define (arrange-coins n)
  (let rc ([k 0])
    (let ([calc (calc-arithmetic k)])
      (if (>= calc n)
          (if (= calc n)
              k
              (sub1 k))
          (rc (add1 k))))))


(arrange-coins 5)
(arrange-coins 8)
(arrange-coins 13)

;; now again, with binary search
;; essentially we want to take a bound. Since (calc z) = z(z+1)/2, we can safely take z/2 as our upper bound to start
;; then we can do a binary search on [1, z/2] searching for the output number k where (calc (sub1 k)) < z <= (calc k)

(define (calc-arithmetic n)
  (/ (* n (add1 n)) 2))

(define (search-for-k z)
  (define (ptr-narrow left right)
    (let* ([k (quotient (+ left right) 2)]
           [k-val (calc-arithmetic k)])
      (cond [(= k-val z) k]
            [(and (> k-val z) (> z (calc-arithmetic (sub1 k)))) (sub1 k)]
            [(> k-val z) (ptr-narrow left (sub1 k))]
            [else (ptr-narrow (add1 k) right)])))
  (ptr-narrow 0 (add1 (quotient z 2))))

(define (arrange-coins n)
  (search-for-k n))

(arrange-coins 5)
(arrange-coins 1)
(arrange-coins 2)
(arrange-coins 3)
(arrange-coins 10)
(arrange-coins 1681692777)

;; with math
(define (arrange-coins n)
  (inexact->exact
   (floor (- (sqrt (+ (* 2 n) (/ 1 4)))
             (/ 1 2)))))

(arrange-coins 12)
