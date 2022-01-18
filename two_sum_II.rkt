#lang racket
(require racket)

;; two pointers
(define (two-sum numbers target [l 0] [r (sub1 (length numbers))])
  (if (< l r)
      (let ([s (+ (list-ref numbers l) (list-ref numbers r))])
        (cond [(= s target) (map add1 (list l r))]
              [(< s target) (two-sum numbers target (add1 l) r)]
              [else (two-sum numbers target l (sub1 r))])) #f))

(two-sum '(2 7 11 15) 9)
(two-sum '(2 3 4) 6)
(two-sum '(-1 0) -1)
