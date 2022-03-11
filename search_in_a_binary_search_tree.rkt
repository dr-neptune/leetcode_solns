#lang racket
(require racket)

(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))


(define (search-bst root val)
  (cond [(not root) #f]
        [(equal? (tree-node-val root) val) root]
        [(< (tree-node-val root) val)
         (search-bst (tree-node-right root) val)]
        [else (search-bst (tree-node-left root) val)]))

(define extree
  (tree-node 4
             (tree-node 2
                        (make-tree-node 1)
                        (make-tree-node 3))
             (make-tree-node 7)))


(search-bst extree 2)
