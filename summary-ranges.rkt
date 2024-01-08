#lang racket
(require racket)

(require (only-in srfi/1 map))

(define (intervals->string intervals)
  (for/list ([r intervals])
    (match r
      [(list a) (number->string a)]
      [(list a b ... c) (string-join (map number->string (list a c)) "->")])))

(define (summary-ranges nums)
  (define contiguous->string
    (compose intervals->string
             (curry filter (compose not empty?))
             reverse
             (curry map reverse)))
  (match nums
    ['() '()]
    [_
     (for/fold ([acc '()]
                [numbers (rest nums)]
                [run (list (first nums))]
                #:result (contiguous->string (cons run acc)))
               ([diff (map (λ (a b) (- b a)) nums (rest nums))])
       (match-let ([(list* fnum rnum) numbers])
         (if (equal? diff 1)
             (values acc rnum (cons fnum run))
             (values (cons run acc) rnum (list fnum)))))]))
