#lang racket
(require racket)

;; try on own

#|

To solve a DP problem, we need to combine 3 things:

1. A function or data structure that will compute/contain the answer
   to the problem for every given state

we will use an array of sums?

2. A recurrence relation to transition between states

f(n) = f(n-2) or f(n-3)

3. base cases, so that our recurrence relation doesn't go on
   infinitely

going backwards, base case is that there are no values left

|#



(let ([nums '(1 2 3 1)])
  (let loop ([vals nums])
    (displayln vals)
    (cond [(< (length vals) 3) 0]
          [(cons
            (+ (first vals) (loop (rest (rest vals))))
            (+ (first vals) (loop (rest (rest (rest vals))))))])))

;; not quite

#|

recurrence relation
dp(i) = max(dp(i - 1), dp(i - 2) + nums(i))

base cases
dp(0) = nums(0)
dp(1) = max(nums(0), nums(1))

|#

;; this works, but it ain't dp if it ain't memoized
(let ([nums '(2 7 9 3 1)])
  (let loop ([money nums])
    (displayln money)
    (match money
      [(list a) a]
      [(list a b) (max a b)]
      [(list a _ ..2)
       (max (loop (rest money))
            (+ (loop (rest (rest money))) a))])))


;; this works, time limit exceeded
(let ([nums '(2 7 9 3 1)])
  (let ([hsh (make-hash)])
    (define (memo-check k value)
      (if (hash-has-key? hsh k)
          (hash-ref hsh k)
          (begin
            (hash-set! hsh k value)
            (displayln (format "money: ~a\thsh: ~a" k hsh))
            value)))
    (let loop ([money nums])
      (match money
        [(list a) a]
        [(list a b) (max a b)]
        [(list a _ ..2)
         (memo-check money
                     (max (loop (rest money))
                          (+ (loop (rest (rest money))) a)))]))))




(let ([nums '(2 7 9 3 1)])
  (define hsh (make-hash))
  (define (dp i)
    (match i
      [0 (list-ref nums 0)]
      [1 (apply max nums)]
      [_
       (begin
         (when (not (hash-has-key? hsh i))
           (hash-set! hsh i (max (dp (sub1 i))
                                 (+ (dp (- i 2)) (list-ref nums i)))))
         (hash-ref hsh i))]))
  (dp (sub1 (length nums))))


(define (rob nums)
  (define hsh (make-hash))
  (define (dp i)
    (match i
      [0 (list-ref nums 0)]
      [1 (apply max (take nums 2))]
      [_
       (begin
         (when (not (hash-has-key? hsh i))
           (hash-set! hsh i (max (dp (sub1 i))
                                 (+ (dp (- i 2)) (list-ref nums i)))))
         (hash-ref hsh i))]))
  (dp (sub1 (length nums))))


(define (rob nums)
  (define nums-vec (list->vector nums))
  (define hsh (make-hash))
  (define (dp i)
    (match i
      [0 (vector-ref nums-vec 0)]
      [1 (apply max (vector->list (vector-take nums-vec 2)))]
      [_
       (begin
         (when (not (hash-has-key? hsh i))
           (hash-set! hsh i (max (dp (sub1 i))
                                 (+ (dp (- i 2)) (vector-ref nums-vec i)))))
         (hash-ref hsh i))]))
  (dp (sub1 (vector-length nums-vec))))

(rob '(2 7 9 3 1))
