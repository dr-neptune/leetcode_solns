#lang racket
(require racket)

(define (is-same-tree p q)
  (cond [(and (not p)(not q)) #t]
        [(or (not p)(not q)) #f]
        [else
         (and (= (tree-node-val p) (tree-node-val q))
              (is-same-tree (tree-node-left p) (tree-node-left q))
              (is-same-tree (tree-node-right p) (tree-node-right q)))]))


(define extree1 (tree-node 1 (make-tree-node 2) #f))
(define extree2 (tree-node 1 #f (make-tree-node 2)))

(is-same-tree extree1 extree1)
(is-same-tree extree1 extree2)

(define (is-same-tree p q)
  (equal? p q))

(is-same-tree extree1 extree2)
(is-same-tree extree1 extree1)
