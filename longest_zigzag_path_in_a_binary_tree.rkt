#lang racket
(require racket)

(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

;; idea
;; keep a max countr
;; keep a counter and keep track of most recent direction
;; if opposite direction is false, cut off the count

(define extree (tree-node 1
                          #f
                          (tree-node 1
                                     (make-tree-node 1)
                                     (tree-node 1
                                                (tree-node 1
                                                           #f
                                                           (tree-node 1
                                                                      #f
                                                                      (make-tree-node 1)))
                                                (make-tree-node 1)))))


(let ([root extree])
  (define (dfs node curr-sum dir)
    (if (not (tree-node? node))
        0
        (match dir
          ['l (dfs (tree-node-left node) (add1 curr-sum) 'r)]
          ['r (dfs (tree-node-right node) (add1 curr-sum) 'l)])))
  )



(define (dfs node curr-sum dir)
    (if (not (tree-node? node))
        (sub1 curr-sum)
        (begin
          (displayln (format "~a ~a ~a" node curr-sum dir))
          (match dir
            ['l (dfs (tree-node-left node) (add1 curr-sum) 'r)]
            ['r (dfs (tree-node-right node) (add1 curr-sum) 'l)]))))



(dfs extree 0 'r)
(dfs (tree-node-right extree) 0 'r)


(define (outer root)
  (cond [(not (tree-node? root)) 0]
        [else (max (dfs root 0 'r)
                   (outer (tree-node-left root))
                   (outer (tree-node-right root)))]))

(max 1 2 3)

(outer extree)


(define (longest-zig-zag root)
  (define (dfs node curr-sum dir)
    (if (not (tree-node? node))
        (sub1 curr-sum)
        (match dir
          ['l (dfs (tree-node-left node) (add1 curr-sum) 'r)]
          ['r (dfs (tree-node-right node) (add1 curr-sum) 'l)])))
  (define (outer root)
    (cond [(not (tree-node? root)) 0]
          [else (max (dfs root 0 'r)
                     (dfs root 0 'l)
                     (outer (tree-node-left root))
                     (outer (tree-node-right root)))]))
  (outer root))
