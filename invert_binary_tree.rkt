#lang racket
(require racket)

(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

(define extree (tree-node 4
                          (tree-node 2
                                     (make-tree-node 1)
                                     (make-tree-node 3))
                          (tree-node 7
                                     (make-tree-node 6)
                                     (make-tree-node 9))))

(define (invert-tree root)
  (match root
    [(tree-node v l r) (tree-node v (invert-tree r) (invert-tree l))]
    [_ root]))

(invert-tree extree)
