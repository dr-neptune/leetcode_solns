#lang racket
(require racket)

;; idea
;; research lol
(define (quickselect l k pivot-fn)
  (if (= 0 (length l))
      (first l)
      (let* ([pivot (pivot-fn (length l))]
             [lows (filter (lambda (x) (< x pivot)) l)]
             [highs (filter (lambda (x) (> x pivot)) l)]
             [pivots (filter (lambda (x) (= x pivot)) l)])
        (cond [(< k (length lows)) (quickselect lows k pivot-fn)]
              [(< k (+ (length lows) (length pivots))) (first pivots)]
              [else (quickselect highs (- k (length lows) (length pivots)) pivot-fn)]))))

(define (quickselect-median l [pivot-fn random])
  (if (= (remainder (length l) 2) 1)
      (quickselect l (quotient (length l) 2) pivot-fn)
      (* 0.5 (+ (quickselect l (sub1 (/ (length l) 2)) pivot-fn)
                (quickselect l (/ (length l) 2) pivot-fn)))))

(define (find-median-sorted-arrays nums1 nums2)
  (quickselect-median (append nums1 nums2)))

(define exls1 '(9 1 0 2 3 4 6 8 7 10 5))

(quickselect-median exls1)

(find-median-sorted-arrays '(1 3) '(2))
(find-median-sorted-arrays '(1 2) '(3 4))
