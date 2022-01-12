#lang racket
(require racket)

(define (leaf-depths tree [count 0])
  (if (not tree)
      (list count)
      (append (leaf-depths (tree-node-left tree) (add1 count))
              (leaf-depths (tree-node-right tree) (add1 count)))))

(define (min-depth root)
  (apply min (leaf-depths root)))
