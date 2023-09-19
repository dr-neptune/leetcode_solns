#lang racket
(require racket)

;; idea
;; for each node, see where it can reach
;; if it can't reach node 0
;; then add it to a list

(define exgraph '((0 1) (1 3) (2 3) (4 0) (4 5)))

(sort exgraph #:key car <)

;; idea
;; if a number can't get there
;; flip the order of the pair
;; then check if it can reach there
;; if not, go to a different node
(define (car-find val ls)
  (findf (λ (pair) (eq? val (car pair))) ls))

;; (define states exgraph)

;; (car-find 0 exgraph)

(define (can-reach-0 node)
  (match node
    [#f #f]
    [(list 0 b) #t]
    [(list a b) (can-reach-0 (car-find b states))]))

(car-find 0 exgraph)

(can-reach-0 '(4 0))
(can-reach-0 '(2 3))
()

(count (compose not identity) (map can-reach-0 exgraph))


(define (car-find val ls)
  (findf (λ (pair) (eq? val (car pair))) ls))

(define (min-reorder n connections)
  (define (can-reach-0 node)
    (match node
      [#f #f]
      [(list 0 b) #t]
      [(list a b) (can-reach-0 (car-find b connections))]))
  (map can-reach-0 connections)
  ;(count (compose not identity) (map can-reach-0 connections))
  )

(define exgraph2 '((1 0) (1 2) (3 2) (3 4)))

(min-reorder 5 exgraph2)



;; (let ([connections exgraph]
;;       [n (length exgraph)])
;;   (define (dfs node)
;;     (let ([seen (make-vector n 0)])
;;       ())))

(can-reach-0 '(1 0))

(min-reorder 5 exgraph2)



(define (car-find val ls)
  (findf (λ (pair) (eq? val (car pair))) ls))

(define (can-reach-0 node states)
  (match node
    [#f #f]
    [(list b 0) #t]
    [(list a b) (can-reach-0 (car-find b states) states)]))



(define (min-reorder n connections)
  (define (can-reach-0 node)
    (match node
      [#f #f]
      [(list 0 b) #t]
      [(list a b) (can-reach-0 (car-find b connections))]))
  ;; idea
  ;; map over nodes
  ;; if we get a #f, try swapping the values and check again
  (let loop ([paths (map can-reach-0 connections)]
             [num-changes 0])
    ;; get indexes
    (let ([f-idx (index-of paths #f)])
      (displayln (format "~a ~a" paths f-idx))
      (match f-idx
        [#f num-changes]
        [_ (loop (map can-reach-0 (list-update connections f-idx reverse))
                 (add1 num-changes))]))))


(can-reach-0 '(1 3) (list-update exgraph 1 reverse))

(min-reorder 6 exgraph)

(list-update '((1 2)(3 4)(5 6)) 1 reverse)

(index-of (map (curryr can-reach-0 exgraph2) exgraph2) #f)



;; retry

;; idea
;; start from 0
;; if 0 is not second, transpose the arrow
;; (0, 1) -> (1, 0)
;; then store the first value and go to whatever nodes connect to it
;; (1, 3)
;; if the first value from the previous node isn't second, transpose
;; (3, 1)
;; and then store the first value and go to whatever nodes connect to it
;; (2, 3)
;; if the first value from the previous node is second then we are good
;; store the first value, and find nodes that contain it
;; if nothing contains it, return #f


(let ([connections exgraph]
      [n 6])
  (define (get-indices node-num)
    (indexes-where connections (λ (pair) (member node-num pair))))
  (let ([graph (list->vector connections)]
        [num-swaps 0])
    (let loop ([relevant-nodes (get-indices 0)]
               [pivot-node 0])
      (if (> pivot-node n)
          num-swaps
          (begin
            (for ([i relevant-nodes])
              (let ([vec-elements (vector-ref graph i)])
                (when (eq? pivot-node (second vec-elements))
                  (begin
                    (vector-set! graph i (reverse vec-elements))
                    (set! num-swaps (add1 num-swaps))
                    (displayln (format "swapped: ~a graph: ~a swaps: ~a" vec-elements graph num-swaps))))))
            (loop (get-indices (add1 pivot-node)) (add1 pivot-node)))))))


(indexes-where exgraph (λ (pair) (member 10 pair)))


;; still start with 0
;; but once we do that, if we swap, loop on the new first value

(let ([connections exgraph]
      [n 6])
  (define (get-indices node-num [ignore #f])
    (let ([idxs (indexes-where connections (λ (pair) (member node-num pair)))])
      (if ignore
          (remove ignore idxs)
          idxs)))
  (let ([graph (list->vector connections)]
        [num-swaps 0])
    (let loop ([relevant-nodes (get-indices 0)]
               [pivot-node 0])
      (if (> pivot-node n)
          num-swaps
          (begin
            (for ([i relevant-nodes])
              (let ([vec-elements (vector-ref graph i)])
                (when (eq? pivot-node (first vec-elements))
                  (begin
                    (vector-set! graph i (reverse vec-elements))
                    (set! num-swaps (add1 num-swaps))
                    (displayln (format "swapped: ~a graph: ~a swaps: ~a" vec-elements graph num-swaps))
                    (loop (get-indices (second vec-elements) i) (second vec-elements)))))))))))

;; now we need to account for the 0 <- 4 -> 5 branch
(let ([connections exgraph]
      [n 6])
  (define (get-indices node-num [ignore #f])
    (let ([idxs (indexes-where connections (λ (pair) (member node-num pair)))])
      (if ignore
          (remove ignore idxs)
          idxs)))
  (let ([graph (list->vector connections)]
        [num-swaps 0])
    (let loop ([relevant-nodes (get-indices 0)]
               [pivot-node 0])
      (if (> pivot-node n)
          num-swaps
          (begin
            (for ([i relevant-nodes])
              (let ([vec-elements (vector-ref graph i)])
                (when (eq? pivot-node (first vec-elements))
                  (begin
                    (vector-set! graph i (reverse vec-elements))
                    (set! num-swaps (add1 num-swaps))
                    (displayln (format "swapped: ~a graph: ~a swaps: ~a" vec-elements graph num-swaps))
                    (loop (get-indices (second vec-elements) i) (second vec-elements)))))))))))

;; we also need to remove the machinery that counts upwards
(let ([connections exgraph]
      [n 6])
  (define (get-indices node-num [ignore #f])
    (let ([idxs (indexes-where connections (λ (pair) (member node-num pair)))])
      (if ignore
          (remove ignore idxs)
          idxs)))
  (let ([graph (list->vector connections)]
        [num-swaps 0])
    (let loop ([relevant-nodes (get-indices 0)])
      (for ([i relevant-nodes])
          (let ([vec-elements (vector-ref graph i)])
            (when (eq? pivot-node (first vec-elements))
              (begin
                (vector-set! graph i (reverse vec-elements))
                (set! num-swaps (add1 num-swaps))
                (displayln (format "swapped: ~a graph: ~a swaps: ~a" vec-elements graph num-swaps))
                (loop (get-indices (second vec-elements) i)))))))))


;; restart
;; try traversing the graph
(define (car-find val ls)
  (findf (λ (pair) (eq? val (car pair))) ls))

(let ([connections exgraph])
  (let loop ([curr-node (first connections)])
    (if curr-node
        (let ([fn (first curr-node)]
              [sn (second curr-node)])
          (displayln (format "visiting: ~a" fn))
          (let ([nn (car-find sn connections)])
            (loop nn)
            ;; (if (not nn)
            ;;     (loop (car-find fn connections))
            ;;     (loop nn))
            ))
        '()
        )))

;; ah wah ah ah ah

;; try again
;; idea
;; for each node
;; see if we can get to 0
;; if we hit a point where we cannot reach 0
;; then swap the pointer
;; then restart

(let ([connections exgraph]
      [n 6]
      [num-flips 0])
  ;; start by just starting at a node and getting to 0
  (define (get-node-graph index)
    (filter (λ (pair) (equal? (first pair) index))
            connections))
  (define (reverse-node! index)
    (set! connections (list-set connections index (reverse (list-ref connections index)))))
  ;; (get-node-graph 4))
  ;; since we will be restarting, if nothing shows up we can just ignore it
  (for/list ([i (in-range n)])
    (let loop ([node-start (get-node-graph i)])
      ;; need to handle each node in node-start
      (for/list ([node node-start])
        (displayln (format "working on: ~a" node))
        (match node
          ['() (begin
                 (reverse-node! i)
                 (set! num-flip (add1 num-flips)))]
          [(list a 0) (begin
                        (displayln "reached 0!")
                        num-flips)]
          [_ (loop (get-node-graph (second node)))])))))

;; this is all kind of a mess
;; make a dfs function
(define (get-node-graph index [connections exgraph])
    (filter (λ (pair) (equal? (first pair) index))
            connections))

(define (dfs node [prev '()])
  (displayln node)
  (match node
    ['() (begin
           (displayln (format "null found! reversing ~a: ~a" prev (reverse prev)))
           (reverse prev))]
    [(list a 0) (begin
                  (displayln "reached 0!")
                  0)]
    [_ (begin
         (displayln (format "map called with ~a" (get-node-graph (second node))))
         (map (λ (node-graph) (dfs node-graph node)) (get-node-graph (second node))))]))

;; now if we hit a null node, then flip the given node


(for/list ([node exgraph]
           [i (in-range (length exgraph))])
  (displayln (format "called ~a times" i))
  (dfs node))

;; try again with backtracking
(define (get-node-graph index [rev #f] [connections exgraph])
  (let ([fn (match rev
              [#f first]
              [_ second])])
    (filter (λ (pair) (equal? (fn pair) index))
            connections)))

(define (dfs node)
  (displayln node)
  (match node
    ['() '()]
    [(list a 0) (begin
                  (displayln "reached 0!")
                  0)]
    [_ (let ([next-node (get-node-graph (second node))])
         (match next-node
           ['() (begin
                  (displayln
                   (format "backtracking away from null node ~a -> ~a" next-node (get-node-graph (second node) #t)))
                  (map dfs (get-node-graph (second node) #t)))]
           [_ (map dfs next-node)]))]))

(for/list ([node exgraph]
           [i (in-range (length exgraph))])
  (displayln (format "called ~a times" i))
  (dfs node))

;; how do I turn a directed graph
;; into an undirected graph?

;; idea
;; take initial graph
;; append it with a reversed graph
;; then try to traverse to an exit
(define (get-node-graph index [prev '()] [connections exgraph])
  (filter (λ (pair) (and (equal? (first pair) index)
                         (not (equal? (second pair) prev))))
          connections))

(let* ([connections exgraph]
       [augmented (append (map (λ (pair) (append pair '(1))) connections)
                          (map (λ (pair) (append (reverse pair) '(0))) connections))])
  (define (dfs node [prev '()])
    ;; need to keep track of prev to always move forward
    (displayln (format "node: ~a" node))
    (let ([next-node (flatten (get-node-graph (second node) (first node) augmented))])
      (displayln (format "next-node: ~a" next-node))
      (match next-node
        ['() 0]
        [_ (+ (last node)
              (map dfs (get-node-graph (second node) (first node) augmented)))])))
  (map dfs (get-node-graph 0 '() augmented)))


;; idea
;; append a 0 to originals
;; and a 1 to augments
;; then sum
;; I think the implementation above is almost there
