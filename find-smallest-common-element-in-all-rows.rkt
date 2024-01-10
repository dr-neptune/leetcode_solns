#lang racket
(require racket)


#|

idea

traverse all lists in turn
if all values are equal return that value
rest lists with lowest value

((1 2 3 4 5)
 (2 4 5 8 10)
 (3 5 7 9 11)
 (1 3 5 7 9))

((2 3 4 5)
 (2 4 5 8 10)
 (3 5 7 9 11)
 (3 5 7 9))


((3 4 5)
 (4 5 8 10)
 (3 5 7 9 11)
 (3 5 7 9))

((4 5)
 (4 5 8 10)
 (5 7 9 11)
 (5 7 9))

((5)
 (5 8 10)
 (5 7 9 11)
 (5 7 9))

-> return 5

|#

(define exmat '((1 2 3 4 5)
                (2 4 5 8 10)
                (3 5 7 9 11)
                (1 3 5 7 9)))

(define (all-equal? ls)
  (match ls
    ['() #f]
    [_ (let ([val (car ls)])
         (andmap (λ (a) (equal? a val)) ls))]))

(define (drop-lowest mat)
  (let ([min-val (apply min (map first mat))])
    (map (λ (ls) (if (equal? (first ls) min-val) (rest ls) ls)) mat)))

(define (smallest-common-element mat)
  (let loop ([mat mat])
    (if (ormap empty? mat) -1
        (let ([firsts (map first mat)])
          (if (all-equal? firsts)
              (first firsts)
              (loop (drop-lowest mat)))))))

;; with sets
(define (smallest-common-element mat)
  (let ([intersection (set->list (apply set-intersect (map (λ (ls) (for/set ([v ls]) v)) mat)))])
    (if (empty? intersection)
        -1
        (apply min intersection))))

;; with hashmap
(define (smallest-common-element mat)
  (let ([intersection
         ((compose hash-keys
                   (λ (ls) (apply hash-intersect ls #:combine +)))
          (map (λ (ls) (for/hash ([v ls]) (values v 0))) mat))])
    (if (empty? intersection)
        -1
        (apply min intersection))))
