#lang racket
(require racket)

(define (max-distance nums1 nums2)
  (let ([nums1 (list->vector nums1)]
        [nums2 (list->vector nums2)])
   (let rc ([results 0]
           [i 0] [j 0])
    (cond [(or (>= i (vector-length nums1)) (>= j (vector-length nums2))) results]
          [(<= (vector-ref nums1 i) (vector-ref nums2 j))
           (rc (max results (- j i)) i (add1 j))]
          [else (rc results (add1 i) (add1 j))]))))
