#lang racket
(require racket)

(define (boolean->int bool) (or (and bool 1) 0))

(define (count-negatives grid)
  (foldl + 0 (map (compose boolean->int negative?) (flatten grid))))

(count-negatives '((4 3 2 -1) (3 2 1 -1) (1 1 -1 -2) (-1 -1 -2 -3)))

(define (count-negatives grid)
  (foldl + 0 (map (λ (row) (count negative? row)) grid)))

;; with binary search
;; since each row is in non-increasing order, we can use binary search to find the first negative element. After that, the rest of the elements are negative.
;; make a fn that finds that and returns # neg per row
;; then map over each row
(define (get-negative-in-row row)
  (define (ptr-narrow left right)
    (let* ([mid (quotient (+ left right) 2)]
           [mid-val (list-ref row mid)])
      (cond [(>= left right) (if (>= mid-val 0) 0
                                 (- (length row) mid))]
            [(>= mid-val 0) (ptr-narrow (add1 mid) right)]
            [else (ptr-narrow left (sub1 right))])))
  (ptr-narrow 0 (sub1 (length row))))

(define (count-negatives grid)
  (foldl + 0 (map get-negative-in-row grid)))


(define exmat '((4 3 2 -1) (3 2 1 -1) (1 1 -1 -2) (-1 -1 -2 -3)))
(count-negatives exmat)
(count-negatives '((3 2)(1 0)))
