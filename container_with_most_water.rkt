#lang racket
(require racket)

;; idea
;; we want to find the combination [a, b] such that a >= b or b >= a
;; and (* (height (min a b)) (distance a b)) is maximized

(define exls '(1 8 6 2 5 4 8 3 7))

;; soln
(define (get-area a b distance)
  (* (min a b) distance))

(define (sliding-window ls n [vals '()])
  (if (= n (length ls))
      (reverse (cons ls vals))
      (sliding-window (rest ls) n (cons (take ls n) vals))))

(define (get-endcaps ls)
  (append (list (first ls)) (list (last ls))))

(define (area-sets ls n)
  (map (lambda (pair)
         (get-area (first pair) (last pair) (sub1 n)))
       (map get-endcaps (sliding-window ls n))))

(define (max-area height)
  (if (= (length height) 2)
      (get-area (first height) (last height) 1)
      (foldl max 0 ((compose flatten stream->list stream-map)
                    (curry area-sets height)
                    (in-inclusive-range 2 (length height))))))

;; the soln above got a time limit exceeded

;; pointers soln
(define (max-area height)
  (define vec (list->vector height))
  (define (areas l r)
    (define (get-area f l len) (* (min f l) len))
    (if (>= l r) 0
        (let ([area (get-area (vector-ref vec l) (vector-ref vec r) (- r l))])
          (max area (if (< (vector-ref vec l) (vector-ref vec r))
                        (areas (add1 l) r)
                        (areas l (sub1 r)))))))
  (areas 0 (sub1 (vector-length vec))))
