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


;; idea
;; iterate through ls and reversed ls
;; when summations for both are equal, return i

(for/last ([lls exls]
           [rls (reverse exls)]))

;; not done!
