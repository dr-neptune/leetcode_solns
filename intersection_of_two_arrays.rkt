#lang racket
(require racket)

;; idea
;; naive implementation
;; check first value in list 1
;; if in list 2, append it to a new list and remove from second list
;; continue until list 1 is empty

(define (intersect nums1 nums2 [results '()])
  (cond [(empty? nums1) results]
        [(member (first nums1) nums2)
         (intersect (rest nums1) (remove (first nums1) nums2) (append results (list (first nums1))))]
        [else (intersect (rest nums1) nums2 results)]))


(define exls1 '(1 2 2 1))
(define exls2 '(2 2))

(intersect exls1 exls2)
(intersect '(4 9 5) '(9 4 9 8 4))
