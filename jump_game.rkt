#lang racket
(require racket)

(define exin '(2 3 1 1 4))
(define exin2 '(3 2 1 0 4))
(define exin3 '(0))
(define exin4 '(2 0 0))

(define exin5
  '(2 0 6 9 8 4 5 0 8 9 1 2 9 6 8 8 0 6 3 1 2 2 1 2 6 5 3 1 2 2 6 4 2 4 3 0 0 0 3 8 2 4 0 1 2 0 1 4 6 5 8 0 7 9 3 4 6 6 5 8 9 3 4 3 7 0 4 9 0 9 8 4 3 0 7 7 1 9 1 9 4 9 0 1 9 5 7 7 1 5 8 2 8 2 6 8 2 2 7 5 1 7 9 6))

#|
idea

start from final index

then we take 1 -> jump_size steps back (recursing)
at each step do the same
if any steps hit index 0, return #t
|#

(let ([nums exin5])
  (let loop ([ls nums])
    (let ([jump-size (first ls)])
      (displayln (format "~a ~a" jump-size ls))
      (cond [(zero? jump-size) #f]
            [(<= (sub1 (length ls)) jump-size) #t]
            [else
             (stream-ormap (λ (j) (loop (drop ls j)))
                           (in-inclusive-range 1 jump-size))]))))



;; currently at time limit exceeded


(define (can-jump nums)
  (if (equal? nums '(0)) #t
      (let loop ([ls nums])
        (let ([jump-size (first ls)])
          (cond [(zero? jump-size) #f]
                [(<= (sub1 (length ls)) jump-size) #t]
                [else
                 (stream-ormap (λ (j) (loop (drop ls j)))
                               (in-inclusive-range 1 jump-size))])))))



#|

so, currently we are processing each 1->jump length jumps
we can cut down this time by doing a depth first search
i.e. take the max jump length, see if we finish
then max jump length - 1, -2, etc

we should remove any 0s?
^ definitely not

maybe try with a for loop and a break condition?

|#

(let ([nums exin5])
  (let loop ([ls nums])
    (let ([jump-size (first ls)])
      (displayln (format "~a ~a" jump-size ls))
      (cond [(<= (sub1 (length ls)) jump-size) #t]
            [(zero? jump-size) #f]
            [else
             (stream-ormap (λ (j) (loop (drop ls j)))
                           (in-inclusive-range (add1 jump-size) 1 -1))]))))


(let ([nums exin2])
  (let loop ([ls nums])
    (let ([jump-size (first ls)])
      (displayln (format "~a ~a" jump-size ls))
      (cond [(<= (sub1 (length ls)) jump-size) #t]
            [(zero? jump-size) #f]
            [else
             (for/or ([i (in-inclusive-range jump-size 1 -1)])
               (loop (drop ls i)))]))))


(define (can-jump nums)
  (let loop ([ls nums])
    (let ([jump-size (first ls)])
      (cond [(<= (sub1 (length ls)) jump-size) #t]
            [(zero? jump-size) #f]
            [else
             (for/or ([i (in-inclusive-range (add1 jump-size) 1 -1)])
               (loop (drop ls i)))]))))


(define (can-jump nums)
  (let loop ([ls nums])
    (let ([jump-size (first ls)])
      (cond [(<= (sub1 (length ls)) jump-size) #t]
            [(zero? jump-size) #f]
            [else
             (stream-ormap (λ (j) (loop (drop ls j)))
                           (in-inclusive-range (add1 jump-size) 1 -1))]))))

(can-jump exin2)

#| case 2 is broken |#
