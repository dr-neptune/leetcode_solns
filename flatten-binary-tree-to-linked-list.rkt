#lang racket
(require racket (rename-in racket [flatten rkt/flatten]))

(define (preorder-traversal root)
  (if (not (tree-node? root))
      '()
      (append (list (tree-node-val root))
              (preorder-traversal (tree-node-left root))
              (preorder-traversal (tree-node-right root)))))

(define (flatten root)
  (let ([pre (preorder-traversal root)])
    (foldr (λ (v r) (tree-node v #f r)) #f pre)))

(flatten extree)

;; it works, but lc wants an in-place algorithm. Super lame
