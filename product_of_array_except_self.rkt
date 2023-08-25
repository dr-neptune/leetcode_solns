#lang racket
(require racket)

;; if there are 0 0s, we good
;; if there is 1 0, then multiple everything but the 0
;; if there is more than 1 0, everything is 0

(define (product-except-self nums)
  (match (indexes-of nums 0)
    [(list a) (map (λ (v) (if (zero? v) (foldl * 1 (remove 0 nums)) 0)) nums)]
    [(list a ..2) (make-list (length nums) 0)]
    [_ (let ([Π (foldl * 1 nums)])
         (map (λ (v) (/ Π v)) nums))]))

(define exls '(1 2 3 4))
(define exls2 '(-1 1 0 -3 3))
(define exls3 '(1 2 0 4 6 8 0))

(product-except-self exls)
(product-except-self exls2)
(product-except-self exls3)
