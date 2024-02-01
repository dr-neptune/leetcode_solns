#lang racket
(require racket)

#|

idea

dumb first

for each, index-of val >

|#

(define extemp '(73 74 75 71 69 72 76 73))
(define extemp '(30 40 50 60))
(define extemp '(30 60 90))

(let ([temperatures extemp])
  (let loop ([nums temperatures]
             [curr-idx 0]
             [ind '()])
    (if (empty? nums)
        (reverse ind)
        (loop (rest nums)
              (add1 curr-idx)
              (cons
               (let ([next-idx (index-where (rest nums) (λ (v) (> v (first nums))))])
                 (displayln (format "nums: ~a curr-idx: ~a next-idx: ~a ind: ~a" nums curr-idx next-idx ind))
                 (if (not next-idx)
                     0
                     (add1 next-idx)))
               ind)))))

(index-where '(69 72 76 73) (λ (v) (> v 71)))

(define (daily-temperatures temperatures)
  (let loop ([nums temperatures] [curr-idx 0] [ind '()])
    (if (empty? nums)
        (reverse ind)
        (loop (rest nums)
              (add1 curr-idx)
              (cons
               (let ([next-idx (index-where (rest nums) (λ (v) (> v (first nums))))])
                 (if (not next-idx) 0 (add1 next-idx)))
               ind)))))

;; time limit exceeded
