#lang racket
(require racket)

;; check if 2 lists are the same, but for one side every left is a right and vice versa
(define (is-symmetric root)
  (define (compare-trees p q)
    (cond [(and (not p) (not q)) #t]
          [(or (not p) (not q)) #f]
          [else
           (and (= (tree-node-val p) (tree-node-val q))
                (compare-trees (tree-node-left p) (tree-node-right q))
                (compare-trees (tree-node-right p) (tree-node-left q)))]))
  (compare-trees (tree-node-left root) (tree-node-right root)))

(define extree (tree-node 1 (tree-node 2 (make-tree-node 3) (make-tree-node 4)) (tree-node 2 (make-tree-node 4) (make-tree-node 3))))

(define extree2 (tree-node 1 (tree-node 2 #f (make-tree-node 3))
                           (tree-node 2 #f (make-tree-node 3))))

(is-symmetric extree)
(is-symmetric extree2)
