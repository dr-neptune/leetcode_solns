#lang racket
(require racket)

;; 1 pass
;; (1 2 3 4 5) 3
;; use vectors
(require (only-in math/statistics mean))

(define (vrefs vec pos-range)
  (vector-map (curry vector-ref vec) (list->vector pos-range)))

(define (find-max-average nums k)
  (let ([vec (list->vector nums)])
    (let loop ([best 0]
               [sk 0]
               [ek k])
      (cond [(= 1 (vector-length vec)) (vector-ref vec 0)]
            [(> ek (vector-length vec)) best]
            [else
             (let ([curr-mean (mean (vrefs vec (range sk ek)))])
               (if (> curr-mean best)
                   (loop curr-mean (add1 sk) (add1 ek))
                   (loop best (add1 sk) (add1 ek))))]))))


(find-max-average exls 4)
(find-max-average '(5) 1)
(find-max-average '(-1) 1)


(define (find-max-average nums k)
  (let loop ([i 0]
             [M 0])
    (if (= i (- (length nums) k))
        (/ (+ (apply + (take nums k)) M) k)
        (let ([d (- (list-ref nums (+ i k)) (list-ref nums i))])
          (if (> d M)
              (loop (add1 i) d)
              (loop (add1 i) M))))))
