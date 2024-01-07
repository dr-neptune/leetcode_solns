#lang racket
(require racket)

(define exmat '(( 1  2  3  4)
                ( 5  6  7  8)
                ( 9 10 11 12)
                (13 14 15 16)))

(define (spiral-order matrix)
  (match matrix
    ['() '()]
    [(list a) a]
    [(list (list a) ..1) (flatten a)]
    [(list a b) (append a (reverse b))]
    [(list a b ..1 c) #:when (= 2 (length a))
     (append a (flatten (map last b)) (reverse c) (flatten (map first (reverse b))))]
    [(list a b ..1 c)
     (append a
             (flatten (map last b))
             (reverse c)
             (flatten (map first (reverse b)))
             (spiral-order (map (λ (ls) (drop-right (cdr ls) 1)) b)))]))
