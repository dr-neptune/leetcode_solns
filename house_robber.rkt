#lang racket
(require racket)

(define exls '(1 2 3 1))
(define exls2 '(2 7 9 3 1))








;; idea
;; for max volume of houses, you can get the max (odd indices, even indices)
;; but this doesn't account for the case when some combination of the 2 is greater
;; and outweighs the cost of skipping a house
;; example:
;; (1 0 1 3 1 0 1)
;; either 4 or 3
;; but we could do (1 3 1) = 5

;; we want to find the maximum subsequence in the sequence
;; Kadane's algorithm?
;; Kadane's algorithm is useful for a contigious sequence

;; (1 2 3 1)
;; starting with 1 and 2
;; 1: (3 1) (1) (2) -> () (1 3) (2 1) = 4
;; 2: (1) (2) (1 3) -> () (2 1) () = 3

;; (2 7 9 3 1)
;; 2 7
;; 2: (9 3 1) (2) (7) -> (1) (2 9) (3) -> () (2 9 1) () = 12
;; 7: (3 1) (7) (2 9) ->

;; if we do a greedy approach, optimality is not guaranteed
;; so we need to do some kind of DFS style go
;; start on 1st and 2nd values
;; then build out the lists
;; (1 2 3 1)
;; 1: (3 1) (1) -> break into taking the 3 and taking the 1
;;    then we have (1 3) and (1 1)
;;    take the max which is (1 3) = 4
;; 2: (1) (2) -> only have 1 option, so (2 1) = 3

(let ([nums exls3]
      [results '()])
  (let dfs ([nums nums]
            [curr-path '()])
    (displayln (format "curr path: ~a results: ~a" curr-path results))
    (match nums
      ;; at least 3 elements left
      [(list a b c ..1)
       (append (dfs (drop nums 2) (cons a curr-path))
               (dfs (drop nums 3) (cons b curr-path)))]
      [(list a b)
       (cons (cons a curr-path)
             (cons (cons b curr-path)
                   results))]
      ;; 1 element left
      [(list a)
       (cons (cons a curr-path) results)]
      ;; nothing left
      [_ (cons curr-path results)])))


(drop exls 2)

(define exls3 '(1 3 1))

;; some problems with this
;; it doesn't hit all the possibilities
;; an example: '(2 7 9 3 1) -> '((1 9 2) (3 7) (1 7)) doesn't include (2 3)
;; let's try it though

(define (rob nums)
  (let ([results '()])
    (apply max (map (curry apply +)
                    (let dfs ([nums nums]
                              [curr-path '()])
                      (match nums
                        ;; at least 3 elements left
                        [(list a b c ..1)
                         (append (dfs (drop nums 2) (cons a curr-path))
                                 (dfs (drop nums 3) (cons b curr-path)))]
                        [(list a b)
                         (cons (cons a curr-path)
                               (cons (cons b curr-path)
                                     results))]
                        ;; 1 element left
                        [(list a)
                         (cons (cons a curr-path) results)]
                        ;; nothing left
                        [_ (cons curr-path results)]))))))

(rob exls)
(rob exls2)
(rob exls3)

;; maybe? time limit exceeded
;; speed it up

(define (contains-sublist? l)
  (cond ((null? l) #f)
        ((list? (car l)) #t)
        (else (contains-sublist? (cdr l)))))

(contains-sublist? '((('yes))))
(contains-sublist? '(no))


(apply max (flatten (map (λ (val) (if (contains-sublist? val)
                         (map (curry apply +) val)
                         (apply + val)))
            (let ([nums exls2]
                  [results '()])
              (let dfs ([nums nums]
                        [curr-path '()])
                (displayln (format "curr path: ~a results: ~a" curr-path results))
                (match nums
                  ;; at least 3 elements left
                  [(list a b c ..1)
                   (cons (dfs (drop nums 2) (cons a curr-path))
                         (dfs (drop nums 3) (cons b curr-path)))]
                  [(list a b)
                   (cons (cons a curr-path)
                         (cons (cons b curr-path)
                               results))]
                  ;; 1 element left
                  [(list a)
                   (cons (cons a curr-path) results)]
                  ;; nothing left
                  [_ (cons curr-path results)]))))))


(define (contains-sublist? l)
  (cond ((null? l) #f)
        ((list? (car l)) #t)
        (else (contains-sublist? (cdr l)))))

(define (rob nums)
  (define (make-paths nums [curr-path '()] [results '()])
    (match nums
      ;; at least 3 elements left
      [(list a b c ..1)
       (cons (make-paths (drop nums 2) (cons a curr-path))
             (make-paths (drop nums 3) (cons b curr-path)))]
      [(list a b)
       (cons (cons a curr-path)
             (cons (cons b curr-path)
                   results))]
      ;; 1 element left
      [(list a)
       (cons (cons a curr-path) results)]
      ;; nothing left
      [_ (cons curr-path results)]))
  (apply max (flatten (map (λ (val) (if (contains-sublist? val)
                                        (map (curry apply +) val)
                                        (apply + val)))
                           (make-paths nums)))))

(rob exls)
(rob exls2)
(rob exls3)


(define (map* func lst)
  (cond
    ((null? lst) '())
    ((not (list? (car lst)))
     (cons (func (car lst)) (map* func (cdr lst))))
    ((contains-sublist? (car lst))
     (cons (map* func (car lst)) (map* func (cdr lst))))
    (else
     (cons (func (car lst)) (map* func (cdr lst))))))

(map* (curry apply +) '((1 2) (2 3)))


(define (flatmap* fn ls)
  (flatten (map* fn ls)))

(map* (curry apply +) '((((((1 2 3))))) (((2 3 4))) ((5 6) (7 8))))


(define (contains-sublist? l)
  (and (not (null? l))
       (or (list? (car l)) (contains-sublist? (cdr l)))))



(apply max (flatten (map (λ (val) (if (contains-sublist? val)
                         (map (curry apply +) val)
                         (apply + val)))
            (let ([nums exls2]
                  [results '()])
              (let dfs ([nums nums]
                        [curr-path '()])
                (displayln (format "curr path: ~a results: ~a" curr-path results))
                (match nums
                  ;; at least 3 elements left
                  [(list a b c ..1)
                   (cons (dfs (drop nums 2) (cons a curr-path))
                         (dfs (drop nums 3) (cons b curr-path)))]
                  [(list a b)
                   (cons (cons a curr-path)
                         (cons (cons b curr-path)
                               results))]
                  ;; 1 element left
                  [(list a)
                   (cons (cons a curr-path) results)]
                  ;; nothing left
                  [_ (cons curr-path results)]))))))







;; map*
;; idea
;; if the iterate through the list
;; if the first value is a list, recurse on it
;; otherwise apply the fn and try again with the cdr
;; cons along the way
(define (contains-sublist? l)
  (cond ((null? l) #f)
        ((list? (car l)) #t)
        (else (contains-sublist? (cdr l)))))

(define (map* func lst)
  (cond
    ((null? lst) '())
    ((not (list? (car lst)))
     (cons (func (car lst)) (map* func (cdr lst))))
    ((contains-sublist? (car lst))
     (cons (map* func (car lst)) (map* func (cdr lst))))
    (else
     (cons (func (car lst)) (map* func (cdr lst))))))

(define (flatmap* fn ls)
  (flatten (map* fn ls)))

(apply max
       (flatmap* (curry apply +)
      (let ([nums exls2]
            [results '()])
        (let dfs ([nums nums]
            [curr-path '()])
    (displayln (format "curr path: ~a results: ~a" curr-path results))
    (match nums
      ;; at least 3 elements left
      [(list a b c ..1)
       (cons (dfs (drop nums 2) (cons a curr-path))
             (dfs (drop nums 3) (cons b curr-path)))]
      [(list a b)
       (cons (cons a curr-path)
             (cons (cons b curr-path)
                   results))]
      ;; 1 element left
      [(list a)
       (cons (cons a curr-path) results)]
      ;; nothing left
      [_ (cons curr-path results)])))))


(define (contains-sublist? l)
  (cond ((null? l) #f)
        ((list? (car l)) #t)
        (else (contains-sublist? (cdr l)))))

(define (map* func lst)
  (cond
    ((null? lst) '())
    ((not (list? (car lst)))
     (cons (func (car lst)) (map* func (cdr lst))))
    ((contains-sublist? (car lst))
     (cons (map* func (car lst)) (map* func (cdr lst))))
    (else
     (cons (func (car lst)) (map* func (cdr lst))))))

(define (flatmap* fn ls)
  (flatten (map* fn ls)))

(define (rob nums)
  (let ([results '()])
    (define (make-paths nums [curr-path '()])
      (match nums
        ;; at least 3 elements left
        [(list a b c ..1)
         (cons (make-paths (drop nums 2) (cons a curr-path))
               (make-paths (drop nums 3) (cons b curr-path)))]
        [(list a b)
         (cons (cons a curr-path)
               (cons (cons b curr-path)
                     results))]
        ;; 1 element left
        [(list a)
         (cons (cons a curr-path) results)]
        ;; nothing left
        [_ (cons curr-path results)]))
    (apply max (flatmap* (curry apply +) (make-paths nums)))))

(rob exls)
(rob exls2)
(rob exls3)

;; someone elses:
;; very nice
(let ([nums exls2])
  (for/fold ([neighbor 0]
             [house 0]
             #:result house)
            ([n nums])
    (values house (max house (+ neighbor n)))))

;; keep track of the previous 2
;; we want to take the max between (+ (n-2) n) and (n-1)
;; at the end, we want to return the previous value

(let ([nums exls])
  (for/fold ([n-2 0]
             [n-1 0]
             #:result n-1)
            ([n nums])
    (values n-1 (max (+ n-2 n) n-1))))

(define (rob nums)
  (for/fold ([n-2 0]
             [n-1 0]
             #:result n-1)
            ([n nums])
    (values n-1 (max (+ n-2 n) n-1))))

;; look at solution since we aren't speedy enough

#|

Most dynamic programming problems can be approached using the following sequence:

1. Find recursive solution
2. recursive (top down)
3. recursive + memo (top down)

In this case:

|#

;; try again
(define (rob nums)
  #\pass)

(let ([nums exls2])
  )

;; ok, so we want to
;; (2 7 9 3 1)
;;
