#lang racket
(require racket)


#|

idea

in-order traverse tree

apply abs dist
take k values that minimize

|#


(define (inorder-traversal tree)
  (if (not tree)
      '()
      (append (inorder-traversal (tree-node-left tree))
              (list (tree-node-val tree))
              (inorder-traversal (tree-node-right tree)))))

(define extree (tree-node 1 #f (make-tree-node 2)))
(define extarget 3.428571)
(define exk 1)





(let* ([car<=? (λ (x y) (< (car x) (car y)))]
       [hp (make-heap car<=?)])
  (let ([dev-cons (map (λ (v) (cons (abs (- extarget v)) v)) (inorder-traversal extree))])
    (heap-add-all! hp dev-cons))
  (for/list ([_ (in-range 2)]
             [val (in-heap/consume! hp)])
    (cdr val)))

(require data/heap)

(define (inorder-traversal tree)
  (if (not tree)
      '()
      (append (inorder-traversal (tree-node-left tree))
              (list (tree-node-val tree))
              (inorder-traversal (tree-node-right tree)))))

(define (closest-k-values root target k)
  (let* ([car<=? (λ (x y) (< (car x) (car y)))]
         [hp (make-heap car<=?)])
  (let ([dev-cons (map (λ (v) (cons (abs (- target v)) v)) (inorder-traversal root))])
    (heap-add-all! hp dev-cons))
  (for/list ([_ (in-range k)]
             [val (in-heap/consume! hp)])
    (cdr val))))
