#lang racket
(require racket)

;; idea
;; try to get an amortized linear top k match for indices in nums2
;; then match to nums1

;; start off by getting top k in nums2 and recording which index they are in
;; maybe a heap of associative lists?

;; lets merge the nums lists together into alists
;; then we can take the top 3 pairs for both
;; and get the max output
(define exnums1 '(1 3 3 2))
(define exnums2 '(2 1 3 4))
(define exk 3)

(define exnums1 '(4 2 3 1 1))
(define exnums2 '(7 5 10 9 6))
(define exk 1)

;; expects 168
(define exnums1 '(2 1 14 12))
(define exnums2 '(11 7 13 6))
(define exk 3)


(require data/heap)

(define (alist<= x y)
  (<= (cdr x) (cdr y)))

;; (sort '((1 3) (2 4) (3 0) (4 9)) alist<=?)

(map cons exnums1 exnums2)

(let ([nums1 exnums1]
      [nums2 exnums2]
      [k exk])
  (let ([alists (map cons nums1 nums2)]
        [hp (make-heap alist<=)])
    ;; we want to place them in a heap and pop until we get top 3
    (heap-add-all! hp alists)

    ;; remove all but top 3 from nums2
    (for ([_ (- (length alists) k)])
      (heap-remove-min! hp))

    (let* ([hp-vec (heap->vector hp)]
           [nums1-sum (sequence-fold + 0 (vector-map car hp-vec))]
           [nums2-min (sequence-fold min +inf.0 (vector-map cdr hp-vec))])
      (exact-round (* nums1-sum nums2-min)))))


(require data/heap)

(define (alist<= x y)
  (<= (cdr x) (cdr y)))

(define (max-score nums1 nums2 k)
  (let ([alists (map cons nums1 nums2)]
        [hp (make-heap alist<=)])
    ;; we want to place them in a heap and pop until we get top k
    (heap-add-all! hp alists)

    ;; remove all but top 3 from nums2
    (for ([_ (- (length alists) k)])
      (heap-remove-min! hp))

    (let* ([hp-vec (heap->vector hp)]
           [nums1-sum (sequence-fold + 0 (vector-map car hp-vec))]
           [nums2-min (sequence-fold min +inf.0 (vector-map cdr hp-vec))])
      (exact-round (* nums1-sum nums2-min)))))


(let ([nums1 exnums1]
      [nums2 exnums2]
      [k exk])
  (let ([alists (map cons nums1 nums2)]
        [hp (make-heap alist<=)])
    ;; we want to place them in a heap and pop until we get top 3
    (heap-add-all! hp alists)

    ;; remove all but top 3 from nums2
    (for ([_ (- (length alists) k)])
      (heap-remove-min! hp))

    ;; at each step, we have the current min for the next 3 elements

    (let* ([hp-vec (heap->vector hp)]
           [nums1-sum (sequence-fold + 0 (vector-map car hp-vec))]
           [nums2-min (sequence-fold min +inf.0 (vector-map cdr hp-vec))])
      (displayln (format "hp-vec: ~a nums1-sum: ~a nums2-min: ~a" hp-vec nums1-sum nums2-min))
      (exact-round (* nums1-sum nums2-min)))

    ;; new idea
    ;; iterate through the heap
    ;;
    ;; print what is left
    (for ([i (in-heap/consume! hp)])
      (displayln i))
    ))

;; look up neetcode lol
