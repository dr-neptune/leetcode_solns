#lang racket
;; (require racket)

;; (define (sliding-window ls n [vals '()])
;;   (if (= n (length ls))
;;       (reverse (cons ls vals))
;;       (sliding-window (rest ls) n (cons (take ls n) vals))))

;; ;; O(nlgn)
;; ;; sort it, break it down into 2-tuples and check if the vals are all the same
;; (define (can-make-arithmetic-progression arr)
;;   (apply = (map (λ (p) (apply - p)) (sliding-window (sort exls <) 2))))

;; ;; O(n)
;; ;; check that the gap is a whole number
;; ;; then if it is, check that the scaled values are all divisible by the gap
;; (define (can-make-arithmetic-progression arr)
;;   (let* ([min-val (apply min arr)]
;;          [max-val (apply max arr)]
;;          [gap (/ (- max-val min-val) (sub1 (length arr)))])
;;     (cond [(= 0 gap) #t]
;;           [(not (integer? gap)) #f]
;;           [(check-duplicates arr) #f]
;;           [else (apply (curry = 0)
;;                        (map (compose
;;                              (curryr modulo gap)
;;                              (λ (d) (- d min-val)))
;;                             arr))])))


;; again

;; idea
;; sort ls
;; check if diff between each element is consistent
(require (only-in srfi/1 map))  ;; use pair-wise map

(define (all-equal? ls)
  (andmap (λ (a) (equal? a (car ls))) ls))

(define (can-make-arithmetic-progression arr)
  (let ([arr (sort arr <)])
    (all-equal? (map - arr (cdr arr)))))


;; (can-make-arithmetic-progression '(1 3 5))

(module+ test
  (require rackunit)
  (check-true (can-make-arithmetic-progression '(1 2 3)))
  (check-true (can-make-arithmetic-progression '(1 3 5)))
  (check-false (can-make-arithmetic-progression '(1 2 4))))
