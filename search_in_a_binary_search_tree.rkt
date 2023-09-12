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


;; redo!
(let ([tree extree]
      [target 2])
  (let loop ([root tree])
    (if (not (tree-node? root))
        '()
        (let ([val (tree-node-val root)])
          (if (eq? val target)
              root
              (cons (loop (tree-node-left root))
                    (loop (tree-node-right root))))))))

(define (search-bst root val)
  (let loop ([root-node root])
    (if (not (tree-node? root-node))
        '()
        (let ([node-val (tree-node-val root-node)])
          (if (eq? node-val val)
              root-node
              (list (loop (tree-node-left root-node))
                    (loop (tree-node-right root-node))))))))

(search-bst extree 2)

;; this is no good because it doesn't use the binary search tree's inherent properties
(let loop ([root extree]
           [val 2])
  (if (not (tree-node? root))
      '()
      (let ([node-val (tree-node-val root)])
        (match node-val
          [(? (curry eq? val)) root]
          [(? (curry < val)) (loop (tree-node-left root) val)]
          [_ (loop (tree-node-right root) val)]))))

(define (search-bst root val)
  (if (not (tree-node? root))
      #f
      (let ([node-val (tree-node-val root)])
        (match node-val
          [(? (curry eq? val)) root]
          [(? (curry < val)) (search-bst (tree-node-left root) val)]
          [_ (search-bst (tree-node-right root) val)]))))

(search-bst extree 2)
