#lang racket
(require racket)

(define (preorder-traversal root [level 0])
  (if (not (tree-node? root))
      '()
      (cons
       (cons (list (tree-node-val root) level)
             (preorder-traversal (tree-node-left root) (add1 level)))
       (preorder-traversal (tree-node-right root) (add1 level)))))

(define (split-into ls subls-size)
  (if (<= (length ls) subls-size)
      (list ls)
      (append (list (take ls subls-size))
              (split-into (drop ls subls-size) subls-size))))

(define (level-order root)
  (if (not (tree-node? root))
      '()
      (let ([pre (preorder-traversal root)])
        (map (λ (ls) (map first ls))
             (group-by (λ (x) (second x)) (split-into (flatten pre) 2))))))

(define (average-of-levels root)
  (map (λ (ls) (/ (apply + ls) (length ls)))
       (level-order root)))