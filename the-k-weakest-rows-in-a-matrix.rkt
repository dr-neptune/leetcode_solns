#lang racket
(require racket)

;; idea
;; for each row, perform binary search to find the last 1 in each row. The index is the "strength" of the row.
;; then grab the top k row indices

(define exmat '((1 1 0 0 0)
                (1 1 1 1 0)
                (1 0 0 0 0)
                (1 1 0 0 0)
                (1 1 1 1 1)))

(define (row-strength row)
  (define (ptr-narrow left right)
    (let* ([mid (quotient (+ left right) 2)]
           [mid-val (list-ref row mid)])
      (cond [(> left right) left]
            [(= mid-val 1) (ptr-narrow (add1 mid) right)]
            [else (ptr-narrow left (sub1 mid))])))
  (ptr-narrow 0 (sub1 (length row))))

(define (in-order-min-k ls k [inds '()])
  "Finds bottom k values in a list with preference for order"
  (if (> k 0)
      (let ([mindex (index-of ls (apply min ls))])
        (in-order-min-k (list-update ls mindex (λ (v) (+ v 101)))
                        (sub1 k)
                        (append inds (list mindex))))
      inds))

(define (k-weakest-rows mat k)
  (in-order-min-k (map row-strength mat) k))


;; with a priority queue
(require data/heap)

(define (row-strength row)
  (define (ptr-narrow left right)
    (let* ([mid (quotient (+ left right) 2)]
           [mid-val (list-ref row mid)])
      (cond [(> left right) left]
            [(= mid-val 1) (ptr-narrow (add1 mid) right)]
            [else (ptr-narrow left (sub1 mid))])))
  (ptr-narrow 0 (sub1 (length row))))

(define (pop-min heap)
  (let ([min-val (heap-min heap)])
    (begin
      (heap-remove-min! heap)
      min-val)))

(define (k-weakest-rows mat k)
  (let ([heap (make-heap (λ (a b) (if (= (cadr a) (cadr b))
                                      (< (car a) (car b))
                                      (< (cadr a) (cadr b)))))])
    (begin
      (heap-add-all! heap
                     (map (λ (r i) (list i (row-strength r)))
                          mat (inclusive-range 0 (sub1 (length mat))))))
    (for/list ([i (range 0 k)])
      (first (pop-min heap)))))

;; one-liner translation
(define (k-weakest-rows mat k)
  (map car
       (take (sort (map (λ (i x) `(,i . ,(apply + x)))
                        (range (length mat)) mat)
                   < #:key cdr)
             k)))
