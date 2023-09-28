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
(require data/heap)

(let ([nums1 exnums1]
      [nums2 exnums2]
      [k exk])
  (let ([pairs (sort (map cons nums1 nums2) #:key cdr >)]
        [hp (make-heap <)]
        [n1-sum 0])
    (for/fold ([results 0]
               #:result (displayln results))
              ([pair pairs])
      (displayln (format "pair: ~a n1-sum: ~a" pair n1-sum))
      (let ([min-val (cdr pair)]
            [n1 (car pair)])
        (begin
          ;; add n1 to heap
          (heap-add! hp n1)
          ;; get sum
          (set! n1-sum (+ n1-sum n1))
          ;;
          (let ([num-eles (heap-count hp)])
            (cond [(> num-eles k)
                   (begin
                     (let ([n1-min (heap-min hp)])
                       (heap-remove-min! hp)
                       (set! n1-sum (- n1-sum n1-min)))
                     (values results))]
                  [(= num-eles k)
                   (displayln (format "res: ~a n1-sum: ~a min-val: ~a" results n1-sum min-val))
                   (values (max results (* n1-sum min-val)))]))
          (values results))))))


;; ok, so we want to keep a minheap of nums1 that is k elements long
;; and we can just keep cdr pair as the min
;; as we move along, we want to keep the max value of (+ a b c)_1 * min_2
;;

;; making it super complicated
;; due to the mutation
;; make it /simple and clean/


(let ([nums1 exnums1]
      [nums2 exnums2]
      [k exk])
  (let ([pairs (sort (map cons nums1 nums2) #:key cdr >)]
        [hp (make-heap <)]
        [n1-sum 0])
    (for/fold ([results 0]
               #:result (displayln results))
              ([pair pairs])
      (displayln (format "pair: ~a n1-sum: ~a" pair n1-sum))
      (let ([min-val (cdr pair)]
            [n1 (car pair)])
        (begin
          (heap-add! hp n1)
          (set! n1-sum (+ n1-sum n1))
          (when (> (heap-count hp) k)
            (let ([n1-min (heap-min hp)])
              (heap-remove-min! hp)
              (set! n1-sum (- n1-sum n1-min))))
          (if (= (heap-count hp) k)
              (begin
                (displayln (format "res: ~a n1-sum: ~a min-val: ~a" results n1-sum min-val))
                (values (max results (* n1-sum min-val))))
              (values results))))))
          )

(define (max-score nums1 nums2 k)
  (let ([pairs (sort (map cons nums1 nums2) #:key cdr >)]
        [hp (make-heap <)]
        [n1-sum 0])
    (for/fold ([results 0])
              ([pair pairs])
      (let ([min-val (cdr pair)]
            [n1 (car pair)])
          (heap-add! hp n1)
          (set! n1-sum (+ n1-sum n1))
          (when (> (heap-count hp) k)
            (let ([n1-min (heap-min hp)])
              (heap-remove-min! hp)
              (set! n1-sum (- n1-sum n1-min))))
          (if (= (heap-count hp) k)
              (values (max results (* n1-sum min-val)))
              (values results))))))

(max-score exnums1 exnums2 exk)


(define (max-score nums1 nums2 k)
  (define-values (pairs hp n1-sum)
    (values (sort (map cons nums1 nums2) #:key cdr >) (make-heap <) 0))
  (for/fold ([results 0])
            ([pair pairs])
    (let ([min-val (cdr pair)]
          [n1 (car pair)])
      (heap-add! hp n1)
      (set! n1-sum (+ n1-sum n1))
      (when (> (heap-count hp) k)
        (let ([n1-min (heap-min hp)])
          (heap-remove-min! hp)
          (set! n1-sum (- n1-sum n1-min))))
      (if (= (heap-count hp) k)
          (values (max results (* n1-sum min-val)))
          (values results)))))


;; with do!
;; leetcode's version of racket doesn't support this yet
(define (max-score nums1 nums2 k)
  (define-values (pairs hp n1-sum)
    (values (sort (map cons nums1 nums2) #:key cdr >) (make-heap <) 0))
  (for/fold ([results 0])
            ([pair pairs]
             #:do [(match-define (cons n1 min-val) pair)])
      (heap-add! hp n1)
      (set! n1-sum (+ n1-sum n1))
      (when (> (heap-count hp) k)
        (let ([n1-min (heap-min hp)])
          (heap-remove-min! hp)
          (set! n1-sum (- n1-sum n1-min))))
      (if (= (heap-count hp) k)
          (values (max results (* n1-sum min-val)))
          (values results))))
