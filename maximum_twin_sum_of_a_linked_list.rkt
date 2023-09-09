#lang racket
(require racket)

; val : integer?
; next : (or/c list-node? #f)
(struct list-node
  (val next) #:mutable #:transparent)

; constructor
(define (make-list-node [val 0])
  (list-node val #f))

;; idea
;; ll->ls
;; summate both ways
;; return max

(define (generate-ll ls)
  (foldl list-node #f ls))

(define (ll->ls ll)
  (let loop ([ll ll]
             [ls-vals '()])
    (if (list-node-next ll)
        (loop (list-node-next ll) (cons (list-node-val ll) ls-vals))
        (cons (list-node-val ll) ls-vals))))

(define exll (generate-ll '(5 4 2 1)))

(ll->ls exll)

(define (pair-sum head)
  (let loop ([ls (ll->ls head)]
             [sums '()])
    (if (null? ls)
        (apply max sums)
        (let ([sum (+ (first ls)
                      (last ls))])
          (loop (rest (drop-right ls 1))
                (cons sum sums))))))

;; sped up
;; with vector
(define (ll->ls ll)
  (let loop ([ll ll]
             [ls-vals '()])
    (if (list-node-next ll)
        (loop (list-node-next ll) (cons (list-node-val ll) ls-vals))
        (cons (list-node-val ll) ls-vals))))

(define (pair-sum head)
  (let* ([vec (list->vector (ll->ls head))]
         [v-len (vector-length vec)])
    (sequence-fold max 0
     (for/vector ([i (in-range (quotient (vector-length vec) 2))])
      (+ (vector-ref vec i)
         (vector-ref vec (- (sub1 v-len) i)))))))

(pair-sum (generate-ll '(5 4 2 1)))
