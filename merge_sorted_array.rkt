#lang racket
(require racket)

(define exls1 '(1 2 3 0 0 0))
(define m 3)
(define exls2 '(2 5 6))
(define n 3)

;; this is correct, but the answer expects it to be in place and use nums1 as a container
(define (merge nums1 m nums2 n)
  (define (mesh ls1 ls2)
    (cond [(empty? ls1) ls2]
          [(empty? ls2) ls1]
          [(<= (first ls1) (first ls2))
           (cons (first ls1)
                 (mesh (rest ls1) ls2))]
          [else (cons (first ls2) (mesh ls1 (rest ls2)))]))
  (mesh (take nums1 m) nums2))

(merge exls1 m exls2 n)

;; hacky workaround because in-place seems weird to me
