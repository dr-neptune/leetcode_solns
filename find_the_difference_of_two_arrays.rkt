#lang racket
(require racket)

;; idea
;; isn't this the set difference?
(define (find-difference nums1 nums2)
  (let ([s1 (list->set nums1)]
        [s2 (list->set nums2)])
    (map set->list
         (list (set-subtract s1 s2)
               (set-subtract s2 s1)))))

(find-difference '(1 2 3) '(2 4 6))
(find-difference '(1 2 3 3) '(1 1 2 2))
