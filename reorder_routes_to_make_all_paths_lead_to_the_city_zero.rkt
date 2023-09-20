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
      ;; (displayln (format "~a ~a" paths f-idx))
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
                          (map (λ (pair) (append (reverse pair) '(0))) connections))]
       [build-up '()])
  (define (dfs node [prev '()])
    ;; need to keep track of prev to always move forward
    (displayln (format "node: ~a" node))
    (when (not (zero? (last node)))
      (set! build-up (cons (last node) build-up)))
    (let ([next-node (flatten (get-node-graph (second node) (first node) augmented))])
      (displayln (format "next-node: ~a" next-node))
      (match next-node
        ['() '()] ;; (begin
               ;; (set! build-up (cons (last node) build-up))
         ;; (println (format "finished: ~a" build-up))
         ;]
        [_ (begin
             ;; (set! build-up (cons (last node) build-up))
             (displayln (format "current: ~a" build-up))
             (map dfs (get-node-graph (second node) (first node) augmented)))]
        ;; [_ (cons (last node)
        ;;       (map dfs (get-node-graph (second node) (first node) augmented)))]
        )))
  (for ([node (get-node-graph 0 '() augmented)])
    (dfs node))
  build-up
  ;; (map dfs (get-node-graph 0 '() augmented))
  )




;; idea
;; append a 0 to originals
;; and a 1 to augments
;; then sum
;; I think the implementation above is almost there


(let* ([connections exgraph]
       [augmented (append (map (λ (pair) (append pair '(1))) connections)
                          (map (λ (pair) (append (reverse pair) '(0))) connections))]
       [build-up '()])
  (define (dfs node [prev '()])
    ;; need to keep track of prev to always move forward
    (when (not (zero? (last node)))
      (set! build-up (cons (last node) build-up)))
    (let ([next-node (flatten (get-node-graph (second node) (first node) augmented))])
      (match next-node
        ['() '()]
        [_ (for ([node (get-node-graph (second node) (first node) augmented)])
             (dfs node))])))
  (for ([node (get-node-graph 0 '() augmented)])
    (dfs node))
  build-up)


(define (min-reorder n connections)
  (let* ([augmented (append (map (λ (pair) (append pair '(1))) connections)
                            (map (λ (pair) (append (reverse pair) '(0))) connections))]
         [build-up '()])
    (define (get-node-graph index [prev '()])
      (filter (λ (pair) (and (equal? (first pair) index)
                             (not (equal? (second pair) prev))))
              augmented))
    (define (dfs node [prev '()])
      ;; need to keep track of prev to always move forward
      (when (not (zero? (last node)))
        (set! build-up (cons (last node) build-up)))
      (let ([next-node (flatten (get-node-graph (second node) (first node)))])
        (match next-node
          ['() '()]
          [_ (for ([node (get-node-graph (second node) (first node))])
               (dfs node))])))
    (for ([node (get-node-graph 0 '())])
      (dfs node))
    (apply + build-up)))

(min-reorder 6 exgraph)
(min-reorder 5 exgraph2)


;; make it faster!
;; idea
;; use binary search to find nodes on the sorted second vals
;; also use cond and add up the first values
(define (node-binary-search graph)
  )

(define (min-reorder n connections)
  (let* ([augmented (append (map (λ (pair) (cons 1 pair)) connections)
                            (map (λ (pair) (cons 0 pair)) connections))]
         [build-up 0])
    (define (get-node-graph index [prev '()])
      (filter (λ (pair) (and (equal? (second pair) index)
                             (not (equal? (third pair) prev))))
              augmented))
    (define (dfs node [prev '()])
      ;; need to keep track of prev to always move forward
      (when (not (zero? (first node)))
        (set! build-up (add1 build-up)))
      (let ([next-node (flatten (get-node-graph (third node) (second node)))])
        (match next-node
          ['() '()]
          [_ (for ([node (get-node-graph (third node) (second node))])
               (dfs node))])))
    (for ([node (get-node-graph 0 '())])
      (dfs node))
    build-up
    ;; (apply + build-up)
    ))



(define (min-reorder n connections)
  (let* ([augmented (append (map (λ (pair) (cons 1 pair)) connections)
                            (map (λ (pair) (cons 0 (reverse pair))) connections))]
         [build-up '()])
    (define (get-node-graph index [prev '()])
      (filter (λ (pair) (and (equal? (second pair) index)
                             (not (equal? (third pair) prev))))
              augmented))
    (define (dfs node [prev '()])
      (displayln (format "node: ~a build: ~a" node build-up))
      ;; need to keep track of prev to always move forward
      (when (not (zero? (first node)))
        (set! build-up (cons 1 build-up)))
      (let ([next-node (flatten (get-node-graph (third node) (second node)))])
        (match next-node
          ['() '()]
          [_ (for ([node (get-node-graph (third node) (second node))])
               (dfs node))])))
    (for ([node (get-node-graph 0)])
      (dfs node))
    (apply + build-up)))





(define (min-reorder n connections)
  (let* ([augmented (sort (append (map (λ (pair) (cons 1 pair)) connections)
                                  (map (λ (pair) (cons 0 (reverse pair))) connections))
                          #:key cadr <)]
         [build-up '()])
    ;; augmented)
    (define (get-node-graph index [prev '()])
      (filter (λ (pair) (and (equal? (second pair) index)
                             (not (equal? (third pair) prev))))
              augmented))
    (define (dfs node [prev '()])
      (displayln (format "node: ~a build: ~a" node build-up))
      ;; need to keep track of prev to always move forward
      (when (not (zero? (first node)))
        (set! build-up (cons 1 build-up)))
      (let ([next-node (flatten (get-node-graph (third node) (second node)))])
        (match next-node
          ['() '()]
          [_ (for ([node (get-node-graph (third node) (second node))])
               (dfs node))])))
    (for ([node (get-node-graph 0)])
      (dfs node))
    (apply + build-up)))


(min-reorder 6 exgraph)
(min-reorder 5 exgraph2)



(define exgraph3 '((0 0 1) (1 1 0) (1 1 2) (0 2 1) (0 2 3) (1 3 2) (1 3 4) (0 4 3)))

;; need to find a hit
;; then traverse left and right appending while they are the same
;; needs to handle first and last as well
(define (check-vals idx graph direction goal)
  (match (list idx direction)
    [(or (list 0 'left)
         (list (== (sub1 (vector-length graph))) 'right)) '()]
    [_ (let ([get-next-node (λ (idx)
                                 (let ([node (vector-ref graph idx)])
                                   (if (equal? (cadr node) goal)
                                     (cons node (check-vals idx graph direction goal))
                                     '())))])
            (match direction
              ['left (get-next-node (sub1 idx))]
              ['right (get-next-node (add1 idx))]))]))


(check-vals 0 (list->vector exgraph3) 'right 0)
(check-vals 0 (list->vector exgraph3) 'right 1)
(check-vals 0 (list->vector exgraph3) 'left 1)
(check-vals 1 (list->vector exgraph3) 'right 1)
(check-vals 1 (list->vector exgraph3) 'left 1)

(check-vals (length exgraph3) (list->vector exgraph3) 'right 4)



(check-vals 3)

(define (graph-binary-search goal graph [idx (quotient (vector-length graph) 2)])
  (displayln (format "goal: ~a graph: ~a idx: ~a" goal graph idx))
  (let loop ([idx idx])
    (let ([node (vector-ref graph idx)])
      (match (cadr node)
        [(? (curry equal? goal))
         node
         (append
          (check-vals idx graph 'left goal)
          (check-vals idx graph 'right goal)
          (list node))]
      [(? (curry < goal)) (loop (quotient idx 2))]
      [(? (curry > goal)) (loop (- (sub1 (vector-length graph))
                                   (quotient idx 2)))]))))

;; suppose our list is length 8
;; we start out by halving and checking idx 4
;; if our goal is higher than index 4, we need to
;; increase our index by (/ idx 2)
;; else we need to reduce our index by
;; (/ idx 2)

(define (vector-search nums target)
  (define (ptr-narrow left right)
    (let* ([pivot (quotient (+ left right) 2)]
           [pivot-val (vector-ref nums pivot)])
      (cond [(> left right) -1]
            [(= pivot-val target) pivot]
            [(< target pivot-val) (ptr-narrow left (sub1 pivot))]
            [else (ptr-narrow (add1 pivot) right)])))
  (ptr-narrow 0 (sub1 (vector-length nums))))

(define (check-vals idx graph direction goal)
  (match (list idx direction)
    [(or (list 0 'left)
         (list (== (sub1 (vector-length graph))) 'right)) '()]
    [_ (let ([get-next-node (λ (idx)
                                 (let ([node (vector-ref graph idx)])
                                   (if (equal? (cadr node) goal)
                                     (cons node (check-vals idx graph direction goal))
                                     '())))])
            (match direction
              ['left (get-next-node (sub1 idx))]
              ['right (get-next-node (add1 idx))]))]))



(let* ([graph (list->vector exgraph3)]
       [goal 4]
       [found-val (vector-search (vector-map cadr graph) goal)])
  (append
   (list (vector-ref graph found-val))
   (check-vals found-val graph 'right goal)
   (check-vals found-val graph 'left goal)))

(define (graph-binary-search goal graph)
  (let* ([found-val (vector-search (vector-map cadr graph) goal)])
    (append
     (list (vector-ref graph found-val))
     (check-vals found-val graph 'right goal)
     (check-vals found-val graph 'left goal))))

(check-vals (search (map cadr exgraph3) 4))
;; search graph
;; find a winner
;; then check surrounding areas


(list-ref exgraph3 (- (sub1 (length exgraph3))
                      (- (sub1 (length exgraph3))
                      (- (sub1 (length exgraph3))
                         (- (sub1 (length exgraph3))
                            (quotient (length exgraph3) 2))))))

;; currently fails at the right-most index
(graph-binary-search 4 (vector-sort (list->vector exgraph3) #:key cadr <))
(graph-binary-search 3 (vector-sort (list->vector exgraph3) #:key cadr <))
(graph-binary-search 0 (vector-sort (list->vector exgraph3) #:key cadr <))
(graph-binary-search 1 (vector-sort (list->vector exgraph3) #:key cadr <))
(graph-binary-search 0 (vector-sort (list->vector exgraph) #:key cadr <))

(check-vals idx graph direction goal)
(check-vals 0 (list->vector exgraph3) 'right 0)
(check-vals 0 (list->vector exgraph3) 'right 1)
(check-vals 1 (list->vector exgraph3) 'right 1)
(check-vals 1 (list->vector exgraph3) 'left 1)

(check-vals 7 (vector-sort (list->vector exgraph3) #:key cadr <) 'left 3)
(check-vals 4 (vector-sort (list->vector exgraph3) #:key cadr <) 'right 3)

(define (min-reorder n connections)
  (let* ([augmented (list->vector
                     (sort (append (map (λ (pair) (cons 1 pair)) connections)
                                   (map (λ (pair) (cons 0 (reverse pair))) connections))
                           #:key cadr <))]
         [build-up '()])
    ;; (graph-binary-search 0 augmented)))
    (define (get-node-graph index [prev '()])
      (filter (λ (pair) (not (equal? (third pair) prev)))
              (graph-binary-search index augmented)))
    (define (dfs node [prev '()])
      (when (not (zero? (first node)))
        (set! build-up (cons 1 build-up)))
      (let ([next-node (flatten (get-node-graph (third node) (second node)))])
        (match next-node
          ['() '()]
          [_ (for ([node (get-node-graph (third node) (second node))])
               (dfs node))])))
    (for ([node (get-node-graph 0)])
      (dfs node))
    (apply + build-up)))

(min-reorder 6 exgraph)
(min-reorder 5 exgraph2)


;; again!
(define (vector-search nums target)
  (define (ptr-narrow left right)
    (let* ([pivot (quotient (+ left right) 2)]
           [pivot-val (vector-ref nums pivot)])
      (cond [(> left right) -1]
            [(= pivot-val target) pivot]
            [(< target pivot-val) (ptr-narrow left (sub1 pivot))]
            [else (ptr-narrow (add1 pivot) right)])))
  (ptr-narrow 0 (sub1 (vector-length nums))))

(define (check-vals idx graph direction goal)
  (match (list idx direction)
    [(or (list 0 'left)
         (list (== (sub1 (vector-length graph))) 'right)) '()]
    [_ (let ([get-next-node (λ (idx)
                                 (let ([node (vector-ref graph idx)])
                                   (if (equal? (cadr node) goal)
                                     (cons node (check-vals idx graph direction goal))
                                     '())))])
            (match direction
              ['left (get-next-node (sub1 idx))]
              ['right (get-next-node (add1 idx))]))]))

(define (graph-binary-search goal graph)
  (let* ([found-val (vector-search (vector-map cadr graph) goal)])
    (append
     (list (vector-ref graph found-val))
     (check-vals found-val graph 'right goal)
     (check-vals found-val graph 'left goal))))

(define (min-reorder n connections)
  (let* ([augmented (list->vector
                     (sort (append (map (λ (pair) (cons 1 pair)) connections)
                                   (map (λ (pair) (cons 0 (reverse pair))) connections))
                           #:key cadr <))]
         [build-up '()])
    (define (get-node-graph index [prev '()])
      (filter (λ (pair) (not (equal? (third pair) prev)))
              (graph-binary-search index augmented)))
    (define (dfs node [prev '()])
      (when (not (zero? (first node)))
        (set! build-up (cons 1 build-up)))
      (let ([next-node (flatten (get-node-graph (third node) (second node)))])
        (match next-node
          ['() '()]
          [_ (for ([node (get-node-graph (third node) (second node))])
               (dfs node))])))
    (for ([node (get-node-graph 0)])
      (dfs node))
    (apply + build-up)))


;; too slow :(
(define (vector-search nums target)
  (define (ptr-narrow left right)
    (let* ([pivot (quotient (+ left right) 2)]
           [pivot-val (vector-ref nums pivot)])
      (cond [(> left right) -1]
            [(= pivot-val target) pivot]
            [(< target pivot-val) (ptr-narrow left (sub1 pivot))]
            [else (ptr-narrow (add1 pivot) right)])))
  (ptr-narrow 0 (sub1 (vector-length nums))))

(define (check-vals idx graph direction goal)
  (match (list idx direction)
    [(or (list 0 'left)
         (list (== (sub1 (vector-length graph))) 'right)) '()]
    [_ (let ([get-next-node (λ (idx)
                                 (let ([node (vector-ref graph idx)])
                                   (if (equal? (cadr node) goal)
                                     (cons node (check-vals idx graph direction goal))
                                     '())))])
            (match direction
              ['left (get-next-node (sub1 idx))]
              ['right (get-next-node (add1 idx))]))]))

(define (graph-binary-search goal graph)
  (let* ([found-val (vector-search (vector-map cadr graph) goal)])
    (append
     (list (vector-ref graph found-val))
     (check-vals found-val graph 'right goal)
     (check-vals found-val graph 'left goal))))

(define (min-reorder n connections)
  (let* ([augmented (list->vector
                     (sort (append (map (λ (pair) (cons 1 pair)) connections)
                                   (map (λ (pair) (cons 0 (reverse pair))) connections))
                           #:key cadr <))]
         [build-up 0])
    (define (get-node-graph index [prev '()])
      (filter (λ (pair) (not (equal? (third pair) prev)))
              (graph-binary-search index augmented)))
    (define (dfs node [prev '()])
      (when (not (zero? (first node)))
        (set! build-up (add1 build-up)))
      (let ([next-node (flatten (get-node-graph (third node) (second node)))])
        (match next-node
          ['() '()]
          [_ (for ([node (get-node-graph (third node) (second node))])
               (dfs node))])))
    (for ([node (get-node-graph 0)])
      (dfs node))
    build-up))


;; ~.~
