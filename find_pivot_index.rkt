#lang racket
(require racket)

(define exls '(1 7 3 6 5 6))

;; idea
;; accumulate left
;; accumulate right
;; find the index where they match

(define (accumulate proc ls)
  (match ls
    ['() '()]
    [_ (reverse
        (foldl (λ (val acc)
                 (cons (proc (car acc) val)
                       acc))
               (list (car ls))
               (rest ls)))]))

(accumulate + exls)
(accumulate + (reverse exls))

(define exls2 '(1 2 3))

(let ([lls (accumulate + exls2)]
      [rls (accumulate + (reverse exls2))])
  (for/last ([val lls]
             [i (in-naturals)]
             #:final (index-of rls val))
    (add1 i)))

(define (accumulate proc ls)
  (match ls
    ['() '()]
    [_ (reverse
        (foldl (λ (val acc)
                 (cons (proc (car acc) val)
                       acc))
               (list (car ls))
               (rest ls)))]))

(define (pivot-index nums)
  (let ([lls (accumulate + nums)]
        [rls (accumulate + (reverse nums))])
    (or (for/last ([val lls]
               [i (in-naturals)]
               #:final (index-of rls val))
      (add1 i)) 0)))

(pivot-index '(2 1 -1))
(pivot-index '(1 2 3))

;; we need something that handles the strictly to the left of the index clause

;; brute force
;; iterate through list
;; calculate foldl left and foldr
(first (filter number? (for/list ([i (in-range (length exls))])
  (let ([prev (apply + (take exls i))]
        [after (apply + (drop exls (add1 i)))])
    (when (eq? prev after)
      i)))))

(for/list ([i (in-range (length exls))]
      #:do [(define prev (apply + (take exls i)))
            (define after (apply + (drop exls (add1 i))))]
      #:when (eq? prev after))
  i)


(define (pivot-index nums)
  (let ([matches (for/list ([i (in-range (length nums))]
                            #:do [(define prev (apply + (take nums i)))
                                  (define after (apply + (drop nums (add1 i))))]
                            #:when (eq? prev after))
                   i)])
    (match matches
      ['() -1]
      [_ (first matches)])))

(pivot-index exls)
(pivot-index '(1 2 3))
(pivot-index '(2 1 -1))

;; #:do was added in 8.4, LC uses 8.3
(define (pivot-index nums)
  (let ([matches (filter number?
                         (for/list ([i (in-range (length nums))])
                           (let ([prev (apply + (take nums i))]
                                 [after (apply + (drop nums (add1 i)))])
                             (when (eq? prev after)
                               i))))])
    (match matches
      ['() -1]
      [_ (first matches)])))

(pivot-index exls)
(pivot-index '(1 2 3))
(pivot-index '(2 1 -1))

;; so this works, but it is not good because it is an O(n^2) operation
;; try it again by pre-computing the aggregates


(let ([lls (list->vector (accumulate + exls))]
      [rls (list->vector  (accumulate + (reverse exls)))])
  (for/list ([i (in-range (length nums))])
    (when (eq? (- (vector-ref lls i) (vector-ref (list->vector exls) i))
               (- (vector-ref rls i) (vector-ref (list->vector exls) i)))
      i)))


;; idea
;; traverse ls forward and backward, accumulating values

;; (1 7 3 6 5 6)      exls
;; (1 8 11 17 22 28) accum
;; (28 27 20 17 11 6) accum rev

;; (0 0)  0
;; (1)    1

;; (1 7 3 6 5 6)
;; (1, 0, 27)  0
;; (7, 1, 20)  1
;; (3, 8, 17)  2
;; (6, 11, 11) 3

;; cons 0 accum, rest rev accum
;; iter til they match
;; (0 27)
;; (1 20)
;; (8 17)
;; (11 11)


(for/list ([i (in-range (length exls))]
           [lls (list->vector (cons 0 (accumulate + exls)))]
           [rls (list->vector (rest (reverse (accumulate + (reverse exls)))))]
           #:when (eq? lls rls))
  i)


(define (accumulate proc ls)
  (match ls
    ['() '()]
    [_ (reverse
        (foldl (λ (val acc)
                 (cons (proc (car acc) val)
                       acc))
               (list (car ls))
               (rest ls)))]))


(define (pivot-index nums)
  (let ([matches (for/list ([i (in-range (length nums))]
                            [lls (list->vector (accumulate + (cons 0 nums)))]
                            [rls (list->vector (rest (reverse (append (accumulate + (reverse nums)) '(0)))))]
                            ;; #:when (eq? lls rls)
                            )
                   (displayln (format "~a ~a ~a" i lls rls))
                   i)])
    (match matches
      ['() -1]
      [_ (first matches)])))


(pivot-index '(-1 -1 0 1 1 0))

;; include 0s on either side
;; (-1 -1 0 1 1 0)
;; (0 -1 -1 0 1 1 0 0)
;;

;; precompute all the left to right sides
(accumulate + exls)
(accumulate + (reverse exls))

(map - (reverse (accumulate + (reverse exls))) exls)
(map - (accumulate + exls) exls)


(filter number?
        (for/list ([lhs (map - (accumulate + exls) exls)]
                   [rhs (map - (reverse (accumulate + (reverse exls))) exls)]
                   [i (in-range (length exls))])
          (when (eq? lhs rhs)
            i)))


(define (accumulate proc ls)
  (match ls
    ['() '()]
    [_ (reverse
        (foldl (λ (val acc)
                 (cons (proc (car acc) val)
                       acc))
               (list (car ls))
               (rest ls)))]))

(define (pivot-index nums)
  (let ([matches (for/list ([lhs (map - (accumulate + nums) nums)]
                            [rhs (map - (reverse (accumulate + (reverse nums))) nums)]
                            [i (in-range (length nums))]
                            #:when (eq? lhs rhs))
                   i)])
    (match matches
      ['() -1]
      [_ (first matches)])))
