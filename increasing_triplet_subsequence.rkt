#lang racket
(require racket)


(define (sliding-window ls n [vals '()])
  (if (= n (length ls))
      (reverse (cons ls vals))
      (sliding-window (rest ls) n (cons (take ls n) vals))))

(define (increasing-triplet nums)
  (match (length nums)
    [(? (curry > 3)) #f]
    [_ (for/or ([slide (sliding-window nums 3)])
         ((curry apply <) slide))]))

((curry > 3) 1)

(define exls '(1 2 3 4 5))

;; idea
;; sliding window 3
;; check if

(define (sliding-window ls n [vals '()])
  (if (= n (length ls))
      (reverse (cons ls vals))
      (sliding-window (rest ls) n (cons (take ls n) vals))))



(let loop ([slides (sliding-window '(2 1 5 0 4 6) 3)])
  (if ()))

(for/or ([slide (sliding-window '(2 1 5 0 4 6) 3)])
  ((curry apply <) slide))

(for/or ([slide (sliding-window '(1 2 3 4 5) 3)])
  ((curry apply <) slide))

(apply < '(0 4 6))

(module+ test
  (require rackunit)
  (check-true (increasing-triplet '(1 2 3 4 5)))
  (check-true (increasing-triplet '(5 4 3 2 1)))
  (check-true (increasing-triplet '(2 1 5 0 4 6))))
