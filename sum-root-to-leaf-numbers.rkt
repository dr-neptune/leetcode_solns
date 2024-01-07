#lang racket
(require racket)

#|

idea

dfs

take first item, then recurse until we hit a leaf node
build up intermediate lists

then unfold them and take the sum

|#


; val : integer?
; left : (or/c tree-node? #f)
; right : (or/c tree-node? #f)
(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

; Example tree
(define example-tree
  (tree-node 4
             (tree-node 9 (tree-node 5 #f #f) (tree-node 1 #f #f))
             (tree-node 0 #f #f)))

(define (tree-paths tree)
  (define (append-subpaths val subpaths) (map (λ (subpath) (cons val subpath)) subpaths))
  (match tree
    [#f '()]
    [(tree-node v #f #f) (list (list v))]
    [(tree-node v r l)
     (append (append-subpaths v (tree-paths r))
             (append-subpaths v (tree-paths l)))]))

(define digit-list->int (λ (ls) (foldl (λ (digit power) (+ (* 10 power) digit)) 0 ls)))

(define (sum-numbers root) (apply + (map digit-list->int (tree-paths root))))
