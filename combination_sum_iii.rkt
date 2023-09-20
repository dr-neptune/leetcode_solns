#lang racket
(require racket)

(define exk 3)
(define exn 7)

;; idea
;; brute
;; start with n
;; then subtract 1a
;; then subtract 1b
;; then loop through 1-9c to see if n - 1a - 1b - nc = 0
;; if so, append
;; then add1 to b

(let ([k exk]
      [n exn]
      [results '()])
  (for ([i (in-inclusive-range 1 9)])
    ;; probably some logic here to see if we have below 0
    (for ([j (in-inclusive-range (add1 i) 9)])
      ;; similar
      (for ([k (in-inclusive-range (add1 j) 9)])
        (when (zero? (- n i j k))
          (set! results (cons (list i j k) results)))))
    )
  results)

(define (combination-sum3 k n)
  (let ([results '()])
    (for ([i (in-inclusive-range 1 9)])
      (for ([j (in-inclusive-range (add1 i) 9)])
        (for ([k (in-inclusive-range (add1 j) 9)])
          (when (zero? (- n i j k))
            (set! results (cons (list i j k) results))))))
    results))

(combination-sum3 9 3)
(combination-sum3 9 45)

;; oh neat, should we be doing recursion?

(let ([k 9]
      [n 45]
      [results '()])
  ;; base case
  ;; else recurse
  (let loop ([k k]
             [prev-val 1]
             [inter '()])
    (if (equal? k 0)
        (when (zero? (- n (foldl + 0 inter) prev-val))
          (set! results (cons prev-val inter)))
        (for ([a (in-inclusive-range (add1 prev-val) 9)])
          (loop (sub1 k) (add1 prev-val) results)))))
