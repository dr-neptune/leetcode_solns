#lang racket
(require racket)

(define exls '(-4 -1 0 3 10))

;; naive
;; pretty quick, roughly o(n)
(define (sorted-squares nums)
  (sort (map (λ (x) (* x x)) nums) <))

;; o(n) approach
;; apply square o(n)
;; find min value o(n)
;; merge both lists o(n)
(define (merge-sorted ls1 ls2)
  (cond [(empty? ls1) ls2]
        [(empty? ls2) ls1]
        [(> (first ls1) (first ls2))
         (cons (first ls2)
               (merge-sorted ls1 (rest ls2)))]
        [else (cons (first ls1)
                    (merge-sorted (rest ls1) ls2))]))

(define (sorted-squares nums)
  (let* ([sq (map (λ (x) (* x x)) nums)]
         [min-ind (index-of sq (apply min sq))]
         [left-ls (take sq min-ind)]
         [right-ls (drop sq min-ind)])
    (merge-sorted (reverse left-ls) right-ls)))

(sorted-squares exls)

;; two pointers
;; idea
;; set pointers to 0 and sub1 len ls
;; then if the value at left ptr > value at right ptr, cons right ptr val to results
;; else cons left ptr val to results
;; warning: sloooow. This isn't very racket-like
(define (square x) (* x x))

(define (sorted-squares nums)
  (reverse
   (let rc ([left 0] [right (sub1 (length nums))])
     (let ([lv (list-ref nums left)]
           [rv (list-ref nums right)])
       (cond
         [(= left right) (list (square lv))]
         [(< (square lv) (square rv)) (cons (square rv) (rc left (sub1 right)))]
         [else (cons (square lv) (rc (add1 left) right))])))))


(sorted-squares exls)
(sorted-squares '(-7 -3 2 3 11))
(sorted-squares '(-10000 -9999 -7 -5 0 0 10000))
(sorted-squares '(-1))
(sorted-squares '(-5 -3 -2 -1))
