#lang racket
(require racket)


#|

idea
get deepest left-most leaf and deepest right-most leaf and add number of levels

|#

(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

(define extree (tree-node 1
                          (tree-node 2
                                     (make-tree-node 4)
                                     (make-tree-node 5))
                          (make-tree-node 3)))



(let ([root extree])
  (let loop ([root root])
    (match root
      [#f '()]
      [(tree-node a b c)
       (begin
         (append (list a) (loop b))
         (append (list a) (loop c)))])))



(define (postorder-traversal root)
  (if (not (tree-node? root))
      '()
      (append (postorder-traversal (tree-node-left root))
              (postorder-traversal (tree-node-right root))
              (list (tree-node-val root)))))






Hi GPT!

In the racket programming language, how can I get all paths through a tree to leaves?

As an example:

(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

(define extree (tree-node 1
                          (tree-node 2
                                     (make-tree-node 4)
                                     (make-tree-node 5))
                          (make-tree-node 3)))

some fn -> '((1 2 4) (1 2 5) (1 3))




(struct tree-node (val left right) #:mutable #:transparent)

; Constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

(define (get-tree-paths root)
  (let loop ([root root])
    (match root
      [#f '()]
      [(tree-node a #f #f) (list (list a))]
      [(tree-node a b c)
       (let ([left-paths (loop (tree-node-left root))]
             [right-paths (loop (tree-node-right root))]
             [cons-path (curry map (λ (p) (cons a p)))])
         (append (cons-path left-paths)
                 (cons-path right-paths)))])))

(define (diameter-of-binary-tree root)
  (let ([max-len (λ (paths) (if (empty? paths) 0 (apply max (map length paths))))])
    (+ (max-len (get-tree-paths (tree-node-left root)))
       (max-len (get-tree-paths (tree-node-right root))))))


(define extree (tree-node 1 (make-tree-node 2) #f))
(define extree (tree-node 2 (tree-node 3 (make-tree-node 1) #f) #f))

#|

oh damn, the longest path might not include the root

here's the editorial solution

|#

(define (diameter-of-binary-tree root)
  (let ([diameter 0])
    (let loop ([root root])
      (match root
        [#f 0]
        [_
         (let ([left-path (loop (tree-node-left root))]
               [right-path (loop (tree-node-right root))])
           (set! diameter (max diameter (+ left-path right-path)))
           (add1 (max left-path right-path)))]))
    diameter))
