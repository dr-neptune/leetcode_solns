#lang racket
(require racket)

(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))


;; idea
;; traverse along the right side until you hit the bottom
;; then we need to check the ones that can be seen because the left side is deeper

;; so get max rhs depth
;; then get the right-most value at any depth exceeding that

;; alternatively
;; get the max depth
;; then get each value that is rightmost at each depth

;; start with the dumb way, then see if we get screwed
(define (right-side-view root)
  (let loop ([tree root]
             [acc '()])
    (if (not (tree-node? tree))
        (reverse acc)
        (loop (tree-node-right tree)
              (cons (tree-node-val tree) acc)))))


(define extree (tree-node 1
                          (tree-node 2
                                     #f
                                     (make-tree-node 5))
                          (tree-node 3
                                     #f
                                     (make-tree-node 4))))


(right-side-view extree)


;; so we need to find the rightmost value at each level
;; BFS
;; outer iterates down left and right sides
;; inner checks to see

;; start with root
;; check both rhs and lhs
;; if there is a rhs, append rhs and recurse rhs and lhs at the next depth level
;; if there is not a rhs, append lhs and recurse lhs at the next depth level
;; if there is neither, return '()

(define (bfs root [acc '()])
  (displayln (format "root: ~a acc: ~a" root acc))
  (if (not (tree-node? root))
      acc
      (let ([lhs (tree-node-left root)]
            [rhs (tree-node-right root)])
        (displayln (format "~a ~a ~a" acc lhs rhs))
        (match (list lhs rhs)
          [(list #f #f) acc]
          [(list #f b)
           (bfs rhs (cons (tree-node-val b) acc))]
          [(list a #f) (bfs lhs (cons (tree-node-val a) acc))]
          [(list a b)
           (append (bfs rhs (cons (tree-node-val b) acc))
                   (bfs (tree-node-right lhs) (cons (tree-node-val b) acc)))]))))

;; needs to handle tree nodes that are #f
(bfs extree)


;; idea
;; take depth and level
;; recurse left and right
;; if there is a right and a left, then append right
;; then recurse both sides
;; if there is only a right, then append right
;; if there is only a left, tehn append left

(define (bfs root)
  (let loop ([tree root]
             [acc '()])
    (match tree
      [#f (reverse acc)]
      [(tree-node a #f b)
       (loop b (cons a acc))]
      [(tree-node a b #f)
       (loop b (cons a acc))]
      [(tree-node a b c)
       (loop c (cons a acc))])))

(define (right-side-view root)
  (bfs root))

(define extree2 (tree-node 1
                           (make-tree-node 2)
                           #f))


(define (bfs root)
  (let loop ([tree root]
             [acc '()]
             [depth 0])
    (match tree
      [#f (reverse acc)]
      [(tree-node a #f b)
       (loop b (cons (list a depth 'l) acc) (add1 depth))]
      [(tree-node a b #f)
       (loop b (cons (list a depth 'r) acc) (add1 depth))]
      [(tree-node a b c)
       (append (loop c (cons (list a depth 'r) acc) (add1 depth))
               (loop b (cons (list a depth 'l) acc) (add1 depth)))])))

(map first (filter (λ (v) (eq? (third v) 'r)) (bfs extree3)))


(define (right-side-view root)
  (map first (filter (λ (v) (eq? (third v) 'r)) (bfs root))))

(right-side-view extree3)

(define extree3 (tree-node 1
                           (tree-node 2
                                      (make-tree-node 4)
                                      #f)
                           (make-tree-node 3)))

(bfs extree)
(bfs extree3)

;; idea
;; go level by level
;; if a node is the rightmost for a level, append it
;;    this would require tracking depth and location
;;    maybe we could append tags as it goes down?


(define (bfs root)
  (let loop ([tree root]
             [acc '()]
             [depth 0])
    (match tree
      [#f (reverse acc)]
      [(tree-node a #f b)
       (loop b (cons (list a depth 'l) acc) (add1 depth))]
      [(tree-node a b #f)
       (loop b (cons (list a depth 'r) acc) (add1 depth))]
      [(tree-node a b c)
       (append (loop c (cons (list a depth 'r) acc) (add1 depth))
               (loop b (cons (list a depth 'l) acc) (add1 depth)))])))



;; visit the nodes level by level
;; maintain 2 queues, current-level and next-level
;; for each current-level, check if there are L and R child nodes
;; if there is a right child, append it to the next level
;; if there is no right child, but there is a left child, append it to the next-level
;;

(let ([root extree])
  (let outer ([curr-level root])
    (let bfs ([next-level '()]
              [results '()])
      (match next-level
        [#f (reverse results)]
        [(tree-node a #f b)
         (bfs (cons b next-level))]
        [(tree-node a b #f)]
        [(tree-node a b c)]))))


;; idea
;; get current level and build next level
;; for curr level
;;    if there is a right, append it to results
;;    if there is only a left, append it to results
;; recurse both to curr-level
;; if both are empty, return results

(first (let ([root extree3])
  (let bfs ([curr-level root]
            [results '()]
            [depth 0])
    (displayln (format "~a ~a" curr-level results))
    (match curr-level
      [#f results]
      [(tree-node a #f b)
       (bfs b (cons (list a depth) results) (add1 depth))]
      [(tree-node a b #f)
       (bfs b (cons (list a depth) results) (add1 depth))]
      [(tree-node a b c)
       (cons (bfs b results (add1 depth))
             (bfs c (cons (list a depth) results) (add1 depth)))]))))

;; now we can get the max val in the rightmost list
;; and drop everything in the first list that up to / equal to the max in the rightmost
;; then take the max



(define extree3 (tree-node 1
                           (tree-node 2
                                      (tree-node 4
                                                 (tree-node 5
                                                            (make-tree-node 6)
                                                            #f)
                                                 (make-tree-node 7))
                                      (make-tree-node 8))
                           (make-tree-node 3)))


;; idea
;; grab current level by adding it to a queue
;; for each node, see if there is a rightmost node. If so, add it to the queue
;; then add the left node, then iterate to the next node, etc
;; for each level, add the last node to the results


;; we essentially want to iterate through the levels
;; and for each level, take each node and add it to a list
;; then take the right-most value and add it to results

(let ([root extree])
  (let level ([curr-level (list root)]
              [result '()])
    (let bfs ([current curr-level]
              [next-level '()])
      ;; now for each node in curr-level
      ;; we want to see if we can add right -> left to next level
      (if (empty? current)
          (level next-level (cons (last next-level) (tree-node-val result)))
          (match (first current)
            [#f '()]
            [(tree-node a b #f)]
            [(tree-node a #f b)]
            [(tree-node a b c)]
            )))))


(let get-layer ([curr-level (list extree)]
                [queue '()])
  (if (empty? curr-level)
      queue
      (match (first curr-level)
        [#f '()]
        [(tree-node a b #f)
         (get-layer (rest curr-level) (append queue (list b)))]
        [(tree-node a #f b)
         (get-layer (rest curr-level) (append queue (list b)))]
        [(tree-node a b c)
         (get-layer (rest curr-level) (cons b (cons c queue)))])))


(define (get-layer level [queue '()])
  (if (empty? level)
      queue
      (match (first level)
        [(tree-node a #f #f)
         (get-layer (rest level) queue)]
        [(tree-node a b #f)
         (get-layer (rest level) (append queue (list b)))]
        [(tree-node a #f b)
         (get-layer (rest level) (append queue (list b)))]
        [(tree-node a b c)
         (get-layer (rest level) (cons b (cons c queue)))])))




(define (iter-layers initial [results '()])
  (displayln (format "initial: ~a results: ~a" initial results))
  (if (eq? initial '())
      results
      (let ([res (get-layer initial)])
        (if (empty? res)
            (reverse results)
            (iter-layers res (cons (tree-node-val (last res)) results))))))

(define (right-side-view root)
  (if (not (tree-node? root))
      '()
      (cons (tree-node-val root)
        (iter-layers (list root)))))


(right-side-view extree)
(right-side-view extree4)


;; yargh
(define extree4
  (tree-node 3
             (tree-node 1
                        #f
                        (make-tree-node 2))
             (tree-node 5
                        (make-tree-node 4)
                        (make-tree-node 6))))


(define (get-layer level [queue '()])
  (if (empty? level)
      queue
      (match (first level)
        [(tree-node a #f #f)
         (get-layer (rest level) queue)]
        [(tree-node a b #f)
         (get-layer (rest level) (append queue (list b)))]
        [(tree-node a #f b)
         (get-layer (rest level) (append queue (list b)))]
        [(tree-node a b c)
         (get-layer (rest level)
                    (append queue (list b c)))])))

(define (iter-layers initial [results '()])
  ; (displayln (format "initial: ~a results: ~a" initial results))
  (if (eq? initial '())
      results
      (let ([res (get-layer initial)])
        (if (empty? res)
            (reverse results)
            (iter-layers res (cons (tree-node-val (last res)) results))))))

(define (right-side-view root)
  (if (not (tree-node? root))
      '()
      (cons (tree-node-val root)
        (iter-layers (list root)))))

(right-side-view extree)

;; woohoo!
