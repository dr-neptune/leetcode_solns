#lang racket
(require racket)

(define (increasing-triplet nums)
  (let loop ([f +inf.f]
             [s +inf.f]
             [n nums])
    (displayln (format "~a ~a ~a" f s n))
    (cond [(null? n) #f]
          [(<= (first n) f) (loop (first n) s (rest n))]
          [(<= (first n) s) (loop f (first n) (rest n))]
          [else #t])))

(module+ tets
  (require rackunit)
  (check-true (increasing-triplet '(1 2 3 4 5)))
  (check-false (increasing-triplet '(5 4 3 2 1)))
  (check-true (increasing-triplet '(2 1 5 0 4 6))))
