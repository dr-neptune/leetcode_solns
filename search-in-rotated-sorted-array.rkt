#lang racket
(require racket)

;; idea
;; find min, then split the list at that point
;; then binary search both sides for the given value
(define (find-min nums)
  "uses binary search to find a minimum value"
  (let rc ([left 0] [right (sub1 (length nums))])
    (let ([mid (quotient (+ left right) 2)])
      (cond [(>= left right) left]
            [(< (list-ref nums mid) (list-ref nums right))
             (rc left mid)]
            [else (rc (add1 mid) right)]))))

(define (bsearch nums target)
  "checks if a value exists in a list using binary search"
  (if (empty? nums) -1
      (let rc ([left 0] [right (sub1 (length nums))])
        (let* ([mid (quotient (+ left right) 2)]
               [mid-val (list-ref nums mid)])
          (cond [(> left right) -1]
                [(= mid-val target) mid]
                [(> mid-val target) (rc left (sub1 mid))]
                [else (rc (add1 left) right)])))))

(define (search nums target)
  "splits a rotated list into components, then searches both"
  (let* ([rotation-split (find-min nums)]
         [lhs (take nums rotation-split)]
         [rhs (drop nums rotation-split)]
         [lhs-search (bsearch lhs target)]
         [rhs-search (bsearch rhs target)])
      (if (<= 0 rhs-search)
        (+ (length lhs) rhs-search)
        lhs-search)))

;; another approach
(define (search nums target)
  (let rc ([left 0] [right (sub1 (length nums))])
    (let* ([mid (quotient (+ left right) 2)]
           [mid-val (list-ref nums mid)])
        (cond [(> left right) -1]
            [(= mid-val target) mid]
            [(>= mid-val (list-ref nums left))
             (if (and (<= (list-ref nums left) target)
                      (< target mid-val))
                 (rc left (sub1 mid))
                 (rc (add1 mid) right))]
            [else (if (and (< mid-val target)
                           (<= target (list-ref nums right)))
                      (rc (add1 mid) right)
                      (rc left (sub1 mid)))]))))

(search '(4 5 6 7 0 1 2) 0)
(search '(4 5 6 7 0 1 2) 3)
