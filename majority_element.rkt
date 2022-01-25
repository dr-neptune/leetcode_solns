#lang racket
(require racket)


(define (counts nums ht)
  (if (empty? nums)
      ht
      (let ([f (first nums)])
        (if (hash-has-key? ht f)
            (begin (hash-update! ht f add1)
                   (counts (rest nums) ht))
            (begin (hash-set! ht f 1)
                   (counts (rest nums) ht))))))

(define (max-value ht)
  (first (sort (hash->list ht) (lambda (x y) (> (cdr x) (cdr y))))))

(define (majority-element nums)
  (let ([results (counts nums (make-hash))])
    (car (max-value results))))

(majority-element '(3 2 3))
(majority-element '(2 2 1 1 1 2 2))

;; moore-voting algorithm
(define (majority-element nums [cts 0] [rets 0])
  (cond [(empty? nums) rets]
        [(= 0 cts)
         (majority-element (rest nums) cts (first nums))]
        [(not (= (first nums) rets))
         (majority-element (rest nums) (sub1 cts) rets)]
        [else
         (majority-element (rest nums) (add1 cts) rets)]))

(majority-element '(3 2 3))
(majority-element '(2 2 1 1 1 2 2))

;; sorting
(define (majority-element nums)
  (list-ref (sort nums <) (quotient (length nums) 2)))
