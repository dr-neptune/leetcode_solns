#lang racket
(require racket)

(define (accumulate proc ls)
  (match ls
    ['() '()]
    [_ (reverse
        (foldl (λ (val acc)
                 (cons (proc (car acc) val)
                       acc))
               (list (car ls))
               (rest ls)))]))

(define (find-middle-index nums)
  (let ([matches (for/list ([lhs (map - (accumulate + nums) nums)]
                            [rhs (map - (reverse (accumulate + (reverse nums))) nums)]
                            [i (in-range (length nums))]
                            #:when (eq? lhs rhs))
                   i)])
    (match matches
      ['() -1]
      [_ (first matches)])))
