#lang racket
(require racket)

;; kadane's algorithm
(define (max-sub-array nums [best -10000] [current 0])
    (if (empty? nums)
        best
        (let ([current-sum (max (first nums) (+ current (first nums)))])
          (max-sub-array (rest nums) (max best current-sum) current-sum))))

(module+ test
  (require rackunit)
  (check-equal? (max-sub-array '(-2 1 -3 4 -1 2 1 -5 4)) 6)
  (check-equal? (max-sub-array '(-2 -1)) -1)
  (check-equal? (max-sub-array '(-1)) -1))

;; with divide and conquer
(define exls '(-2 1 -3 4 -1 2 1 -5 4))

;; left side
(define middle (quotient (+ 0 (sub1 (length exls))) 2))
(list-tail (take exls middle) 0)

;; right side
(take (list-tail exls (add1 middle)) (- (sub1 (length exls)) middle))

;; not right, will debug another time
(define (max-sub-array nums left right)
  (if (> left right)
      -10000
      (let* ([middle (quotient (+ left right) 2)]
             [middle-num (list-ref nums middle)]
             [left-ls (list-tail (take nums middle) left)]
             [left-sum (apply + (list-tail (take nums middle) left))]
             [right-ls (take (list-tail nums (add1 middle)) (- right middle))]
             [right-sum (apply + (take (list-tail nums (add1 middle)) (- right middle)))])
        (println left-ls)
        (println right-ls)
        (max (max-sub-array nums left (sub1 middle))
             (max-sub-array nums (add1 middle) right)
             (+ left-sum middle-num right-sum)))))

(max-sub-array exls 0 (sub1 (length exls)))

(max-sub-array exls 0 (sub1 (length exls)))

(max-sub-array '(4 -1 2 1) 0 3) ;; this works :/
