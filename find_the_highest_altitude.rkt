#lang racket
(require racket)

;; idea
;; we essentially want to accumulate by adding values
(define exls '(-5 1 5 0 -7))

(define (accumulate proc ls)
  (match ls
    ['() '()]
    [_ (reverse
        (foldl (λ (val acc)
                 (cons (proc (car acc) val)
                       acc))
               (list (car ls))
               (rest ls)))]))

(accumulate + exls)
(accumulate + '(1 2 3 4 5))
(accumulate + '())

(require rebellion/streaming/transducer
         rebellion/collection/list)

(transduce '(1 2 3 4 5)
           (folding + 0)
           #:into into-list)
