#lang racket
(require racket)

#|

idea

make a custom comparator
then merge all strings, sort and remove duplicates

|#

(require rebellion/base/comparator)
(require rebellion/streaming/transducer)
(require rebellion/collection/list)

(define exwords '("wrt" "wrf" "er" "ett" "rftt"))

(define fcomp (apply comparator-of-constants '(#\w #\r #\t)))

(transduce
 (string->list (string-join exwords ""))
 (sorting
  (apply comparator-chain
         (map (curry apply comparator-of-constants)
              (map (compose remove-duplicates string->list) exwords))))
 #:into into-list)

;; not quite, we can't handle missing values

#|

all approaches break the problem into 3 steps

1. Extracting dependency rules from the input. For example A < C, x < D, E < B
2. Putting the dependency rules into a graph with letters as nodes and dependencies as edges (an adjacency list is best)
3. Topologically sorting the graph nodes

|#

(require (only-in srfi/1 map))

(make-hash '([1 . 0]))

(make-hash (map (λ (v) `(,v . 0)) ((compose remove-duplicates flatten (curry map string->list)) exwords)))

;; create counter data structure
(let ([words exwords])
  (define (populate-graph words)
    (define in-degree
      (make-hash (map (λ (v) `(,v . 0)) ((compose remove-duplicates flatten (curry map string->list)) words))))
    (define adj-list (make-hash))
    (for ([pair (map cons words (rest words))])
      (match-let ([(cons fword sword) pair])
        (for ([fchar (string->list fword)]
              [schar (string->list sword)])
          (if (not (equal? fchar schar))
              (when (not (member schar (hash-ref adj-list fchar '())))
                (hash-set! adj-list fchar (cons schar (hash-ref adj-list fchar '())))
                (hash-set! in-degree schar (add1 (hash-ref in-degree schar))))
              ;; check that the second word isn't a prefix of the first word
              (when (< (string-length sword) (string-length fword))
                "")))))
    (cons in-degree adj-list))
  (populate-graph words))

;; populate adj list and in_degree

;; '(#hash((#\r . 1) (#\t . 1) (#\e . 1) (#\f . 1) (#\w . 0)) .
;;   #hash((#\r . (#\t)) (#\t . (#\f)) (#\e . (#\r)) (#\w . (#\e))))


(match-define (cons in-degree adj-list) inter)

(for/list ([(k v) (in-hash in-degree)]
           #:when (zero? v))
  (values k))

#|


    # Step 2: We need to repeatedly pick off nodes with an indegree of 0.
    output = []
    queue = deque([c for c in in_degree if in_degree[c] == 0])
    while queue:
        c = queue.popleft()
        output.append(c)
        for d in adj_list[c]:
            in_degree[d] -= 1
            if in_degree[d] == 0:
                queue.append(d)

    # If not all letters are in output, that means there was a cycle and so
    # no valid ordering. Return "" as per the problem description.
    if len(output) < len(in_degree):
        return ""
    # Otherwise, convert the ordering we found into a string and return it.
    return "".join(output)

|#

(let ([output '()] [queue (for/list ([(k v) (in-hash in-degree)]
                                     #:when (zero? v))
                            (values k))])
  (for ([item queue])
    (set! output (cons item output))
    (for ([d (in-list (hash-ref adj-list item))])
      (hash-set! in-degree d (sub1 (hash-ref in-degree d)))
      (when (zero? (hash-ref in-degree d))
        (set! queue (cons queue (list d))))))
  (values output queue))

;; probably not quite right ^^
