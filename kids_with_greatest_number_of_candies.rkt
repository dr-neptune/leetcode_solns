#lang racket
(require racket)

;; idea
;; get max of initial array
;; map (+ extraCandies v) across array
;; if new value is > initial max value, return #t else #f

(define (kids-with-candies candies extraCandies)
  (let ([initial-max (apply max candies)])
    (map (λ (v) (>= (+ v extraCandies) initial-max)) candies)))

;; optimized with help from Soegaard
(define (kids-with-candies candies extraCandies)
  (let* ([initial-max (foldl max 0 candies)]
         [diff (- initial-max extraCandies)])
    (map (λ (v) (>= v diff)) candies)))

(kids-with-candies (list 2 3 5 1 3) 3)
(kids-with-candies (list 4 2 1 1 2) 1)
(kids-with-candies (list 12 1 12) 10)
