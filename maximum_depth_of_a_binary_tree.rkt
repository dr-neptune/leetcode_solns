#lang racket
(require racket)

(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

(define extree (tree-node 3
                          (make-tree-node 9)
                          (tree-node 20
                                     (make-tree-node 15)
                                     (make-tree-node 7))))

(define (max-depth root)
  (let loop ([root root])
    (match root
      [#f 0]
      [(tree-node v l r)
       (let ([left (add1 (loop l))] [right (add1 (loop r))])
         (max left right))])))
