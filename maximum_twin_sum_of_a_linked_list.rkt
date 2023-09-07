#lang racket
(require racket)

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

;; make twin sums
(define (ll->ls ll)
  (let loop ([ll ll]
             [ls-vals '()])
    (if (list-node-next ll)
        (loop (list-node-next ll) (cons (list-node-val ll) ls-vals))
        (cons (list-node-val ll) ls-vals))))

(define (pair-sum head)
  (let loop ([ls (ll->ls head)]
             [sums '()])
    (if (null? ls)
        (apply max sums)
        (let ([sum (+ (first ls)
                      (last ls))])
          (loop (rest (drop-right ls 1))
                (cons sum sums))))))

;; not done! need to speed up
