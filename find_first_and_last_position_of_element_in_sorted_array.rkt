#lang racket
(require racket)

(define exls '(5 7 7 8 8 10))

;; straight-forward solution
(define (search-range nums target)
  (let ([idx (indexes-of nums target)])
    (if (empty? idx)
        '(-1 -1)
        (list (apply min idx)
              (apply max idx)))))

(search-range exls 8)
(search-range exls 6)
(search-range '() 0)

;; with binary search
