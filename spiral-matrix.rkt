#lang racket
(require racket)

(define exmat '((1 2 3)
                (4 5 6)
                (7 8 9)))


#|

idea

given length m and height n
take m elements
then take n - 1 elements up -> down
then take m - 1 elements right -> left
then take n - 2 elements down -> up
then take m - 2 elements right -> left

|#

(define (get vec x y)
  (vector-ref (vector-ref vec y) x))

(define inter (list->vector (map list->vector exmat)))

(define exmat '((1 2 3 4)
                (5 6 7 8)
                (9 10 11 12)))

(vector-length exmat)

(let* ([matrix (list->vector (map list->vector exmat))]
       [h (vector-length matrix)]
       [l (vector-length (vector-ref matrix 0))])
  (let loop ([m (stream->list (in-range l))]
             [n (stream->list (in-range h))]
             [direction 'right]
             [acc '()])
    (displayln (format "m: ~a\tn: ~a\tdir: ~a\tacc: ~a" m n direction acc))
    (cond [(empty? m) (cons (get matrix (first n) (first n)) acc)]
          [(empty? n) (cons (get matrix (first m) (first m)) acc)]
          [else
           (match direction
             ['right (loop
                      m
                      (rest n)
                      'down
                      (cons (map (λ (x) (get matrix x (first n))) m) acc))]
             ['down (loop
                     (drop-right m 1)
                     (reverse n);; (rest n)
                     'left
                     (cons (map (λ (y) (get matrix (last m) y)) n) acc))]
             ['left (loop
                     m
                     (rest n) ;; (drop-right n 1)
                     'up
                     (cons (reverse (map (λ (x) (get matrix x (first n))) m)) acc))]
             ['up (loop
                   (drop m 1)
                   (drop-right n 1)
                   'right
                   (cons (map (λ (y) (get matrix (first m) y)) n) acc))])])))
