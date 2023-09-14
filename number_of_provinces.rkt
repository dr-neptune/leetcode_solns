#lang racket
(require racket)

(define exgraph '((1 1 0) (1 1 0) (0 0 1)))

;; idea
;; this is like the rooms and keys problem
;; we dfs on each room
;; in doing so, we create a data structure of a province
;; at the end, we could how many provinces there are

;; each node is a node's connection to others
;; ex: (0 0 0 1 1 0 0 0) means we have 8 nodes, and this node is either 4 or 5 and connected to whichever node it isn't

;; idea
;; make an enumeration of each node
;; go to node 1
;; dfs
;; for each 1 in node 1, add it to a list
;;    for each node in the list recurse and add new items to that list
;;      if that node hasn't been seen before
;; for the first node that isn't in the first dfs repeat the process
;; when the other list is empty, count up the unique sets

;; we are trying to make n coverings
;; try just on node 1
(let ([nodes exgraph])
  (let dfs ([node (first nodes)]
            [seen (make-vector (length (first nodes)) 0)])
    (let loop ([vals node]
               [i (stream->list (in-range (length node)))])
      ;; loop through 1s and 0s
      (cond [(empty? vals) '()]
            [(zero? (first vals))
             (loop (rest vals) (add1 i))]
            [else (begin
                    (vector-set! seen i 1)
                    (loop (list-ref nodes i) (add1 i)))]))))

(let* ([nodes exgraph]
       [seen (make-vector (length (first nodes)) 0)])
  (let loop ([fnode (third nodes)])
    (displayln fnode)
    (for/list ([val fnode]
               [i (in-range (length fnode))])
      ;; check if seen
      (match val
        [1 (if (zero? (vector-ref seen i))
          (begin
            (vector-set! seen i 1)
            (loop (list-ref nodes i)))
          #f)]
        [_ #f])
      ))
  seen)
