#lang racket
(require racket)

#|

idea

match-let on direction amount

if need to shift on a circular array

|#

(for/list ([char (in-cycle (string->list "abc"))]
           [idx (in-range 10)])
  char)

;; left_idx = length str - left

(let ([s "abcdefg"]
      [shift '((1 1) (1 1) (0 2) (1 3))])
  (let* ([strls (string->list s)]
         [str-len (length strls)])
    (for/fold ([strls strls]
               #:result (list->string strls))
              ([shift-pair shift])
      (displayln (format "~a ~a" strls shift-pair))
      (match-let ([(list dir mag) shift-pair])
        (match dir
          [0 (let ([idx (- str-len mag)])
               (rest (list-insert strls idx (first strls))))]
          [1 (rest (list-insert strls mag (first strls)))])))))


(define (list-insert ls pos v)
  (let loop ([ls ls]
             [idx 0])
    (cond [(empty? ls) '()]
          [(equal? idx pos) (cons (first ls) (cons v (rest ls)))]
          [else
           (cons (first ls) (loop (rest ls) (add1 idx)))])))

(list-insert '(1 2 3 4 5) 3 8)


#|

idea

generate in a cycle
if we move right, just move the window to the right with drop
if we move left, move the window to the left with a right shift @ length str + mag

|#

;; shift to the left
(drop
 (for/list ([f (in-cycle (string->list "abc"))]
           [idx (in-range (+ 3 1))])
   f)
 1)

;; shift to the right
(drop
  (for/list ([f (in-cycle (string->list "bca"))]
             [idx (in-range (+ 3 (+ 1 3)))])
    f)
  (+ 3 1))



(drop-right
  (for/list ([f (in-cycle (string->list "abcdefg"))]
             [idx (in-range (+ 7 (+ 1 7)))])
    f)
  (+ 7 1))

#|

idea

use match

|#
;; shift right
(define (shift-right ls)
  (match ls
    [(list a ... b) (cons b a)]))

(define (shift-left ls)
  (match ls
    [(list* a b) (append b (list a))]))

(define (repeat fn times initial-val)
  (for/fold ([result initial-val])
            ([idx (in-range times)])
    (fn result)))

(repeat shift-right 2 (string->list "abcdefg"))


(let ([s "abcdefg"]
      [shift '((1 1) (1 1) (0 2) (1 3))])
  (let* ([strls (string->list s)])
    (for/fold ([strls strls]
               #:result (list->string strls))
              ([shift-pair shift])
      (displayln (format "~a ~a" strls shift-pair))
      (match-let ([(list dir mag) shift-pair])
        (match dir
          [0 (repeat shift-left mag strls)]
          [1 (repeat shift-right mag strls)])))))

(let ([s "abc"]
      [shift '((0 1) (1 2))])
  (let* ([strls (string->list s)])
    (for/fold ([strls strls]
               #:result (list->string strls))
              ([shift-pair shift])
      (displayln (format "~a ~a" strls shift-pair))
      (match-let ([(list dir mag) shift-pair])
        (match dir
          [0 (repeat shift-left mag strls)]
          [1 (repeat shift-right mag strls)])))))


(define (shift-right ls)
  (match ls
    [(list a ... b) (cons b a)]))

(define (shift-left ls)
  (match ls
    [(list* a b) (append b (list a))]))

(define (repeat fn times initial-val)
  (for/fold ([result initial-val])
            ([idx (in-range times)])
    (fn result)))

(define (string-shift s shift)
  (let* ([strls (string->list s)])
    (for/fold ([strls strls]
               #:result (list->string strls))
              ([shift-pair shift])
      (match-let ([(list dir mag) shift-pair])
        (match dir
          [0 (repeat shift-left mag strls)]
          [1 (repeat shift-right mag strls)])))))
