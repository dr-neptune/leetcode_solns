#lang racket
(require racket)


#|

idea

need to handle duplicates

have a loop through nums2 counting indices and pass it a list of already occuring indices in the mapping
if an index in in the already occuring indices, skip it


maybe we can use indexes-of combined with a vector / updating in place?

|#

(let ([nums1 '(12 28 46 32 50)]
      [nums2 '(50 12 32 46 28)])
  (for ([num nums1])
    (let ([ls-indices (indexes-of nums1 num)]
          [other-indices (indexes-of nums2 num)])
      (for ([curr ls-indices]
            [other other-indices])
        (set! nums1 (list-set nums1 curr other)))))
  nums1)


(let ([nums1 '(21 5 74 5 74 21)]
      [nums2 '(21 5 74 74 5 21)])
  (for ([num nums1])
    (let ([ls-indices (indexes-of nums1 num)]
          [other-indices (indexes-of nums2 num)])
      (for ([curr ls-indices]
            [other other-indices])
        (set! nums1 (list-set nums1 curr other)))))
  nums1)

;; this seems correct, but the autograder isn't accepting it

(define (anagram-mappings nums1 nums2)
  (for ([num nums1])
    (let ([ls-indices (indexes-of nums1 num)]
          [other-indices (indexes-of nums2 num)])
      (for ([curr ls-indices]
            [other other-indices])
        (set! nums1 (list-set nums1 curr other)))))
  nums1)


;; try again with a hash comp
(let ([nums1 '(21 5 74 5 74 21)]
      [nums2 '(21 5 74 74 5 21)])
  (let ([hsh (for/hash ([num nums1])
               (values num (index-of nums2 num)))])
    (for/list ([num nums1])
      (hash-ref hsh num))))

(define (anagram-mappings nums1 nums2)
  (let ([hsh (for/hash ([num nums1])
               (values num (index-of nums2 num)))])
    (for/list ([num nums1])
      (hash-ref hsh num))))
