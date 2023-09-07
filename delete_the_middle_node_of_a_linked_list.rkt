#lang racket
(require racket)

(struct list-node
  (val next) #:mutable #:transparent)

(define (make-list-node [val 0])
  (list-node val #f))

(define (generate-ll ls)
  (foldl list-node #f ls))

(define exll (generate-ll '(1 3 4 7 1 2 6)))

(define (ll-length head)
  (let loop ([ll head]
             [node-count 1])
    (if (list-node-next ll)
        (loop (list-node-next ll) (add1 node-count))
        node-count)))

(ll-length (generate-ll '(1 3 4 7 1 2 6)))

;; now we need to traverse the ll and skip the halfway-node
;; rebuilding as we move along
(let* ([ll-len (ll-length exll)]
       [halfway-node (quotient ll-len 2)])
  )

;; just cause
(define (ll->ls ll)
  (let loop ([ll ll]
             [ls-vals '()])
    (if (list-node-next ll)
        (loop (list-node-next ll) (cons (list-node-val ll) ls-vals))
        (cons (list-node-val ll) ls-vals))))

(define (generate-ll ls)
  (foldl list-node #f ls))

(define (delete-middle head)
  (let* ([ls (ll->ls head)]
         [ls-len (length ls)]
         [halfway (quotient ls-len 2)])
    (generate-ll
     (append (take ls halfway)
             (drop ls (add1 halfway))))))

(delete-middle exll)
(delete-middle (generate-ll '(1 2 3 4)))
(delete-middle (generate-ll '(2 1)))

;; do it again without cheating using racket stdlib
;; not done! getting right answer locally, wrong answer in repl
