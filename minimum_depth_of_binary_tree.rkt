#lang racket
(require racket)

(define (leaf-depths tree [count 0])
  (if (not tree)
      (list count)
      (append (leaf-depths (tree-node-left tree) (add1 count))
              (leaf-depths (tree-node-right tree) (add1 count)))))

(define (leaf-depths tree [count 1])
  (cond [(and (not (tree-node-left tree))
              (not (tree-node-right tree)))
         (list count)]
        [(not (tree-node-left tree))
         (leaf-depths (tree-node-right tree) (add1 count))]
        [(not (tree-node-right tree))
         (leaf-depths (tree-node-left tree) (add1 count))]
        [else
         (append (leaf-depths (tree-node-left tree) (add1 count))
                 (leaf-depths (tree-node-right tree) (add1 count)))]))

(define (min-depth root)
  (if (not (tree-node? root))
      0
      (apply min (leaf-depths root))))

;; another version
(define (min-depth root)
  (if (not (tree-node? root))
      0
      (let ([left (min-depth (tree-node-left root))]
            [right (min-depth (tree-node-right root))])
        (if (or (= 0 left) (= 0 right))
            (add1 (+ left right))
            (add1 (min left right))))))
