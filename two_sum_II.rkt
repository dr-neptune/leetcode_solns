#lang racket
(require racket)

(define (two-sum numbers target)
  (let ([results (make-hash)])
    (for-each
     (lambda (v)
       (let ([val (hash-ref results (first v) #f)])
         (if val
             (list (add1 val) (second v))
             (hash-update! results (- target val) (second v)))))
     (enumerate numbers))))

(two-sum '(2 7 11 15) 9)

(define (enumerate ls [idx 0])
  (if (empty? ls)
      '()
      (cons (list (first ls) idx)
            (enumerate (rest ls) (add1 idx)))))
