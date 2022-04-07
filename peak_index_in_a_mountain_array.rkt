#lang racket
(require racket)

(define exls '(0 1 0))
(define exls2 '(0 2 1 0))

;; o(n) version
(define (peak-index-in-mountain-array arr [idx 0])
  (cond [(> (first arr) (second arr)) idx]
        [else (peak-index-in-mountain-array (rest arr) (add1 idx))]))

(peak-index-in-mountain-array exls)
(peak-index-in-mountain-array exls2)

;; o(logn) version

;; with lists
(define (peak-index-in-mountain-array arr)
  (define (ptr-narrow left right)
    (let* ([pivot (+ left (quotient (- right left) 2))]
           [pivot-val (list-ref arr pivot)]
           [lhs-< (< (list-ref arr (sub1 pivot)) pivot-val)]
           [rhs-< (< (list-ref arr (add1 pivot)) pivot-val)])
      (cond [(and lhs-< rhs-<) pivot]
            [lhs-< (ptr-narrow (add1 pivot) right)]
            [else (ptr-narrow left (add1 pivot))])))
  (ptr-narrow 0 (sub1 (length arr))))

;; the time to convert to vector takes too long
;; therefore, this is o(n) :/
(define (peak-index-in-mountain-array arr)
  (let ([vec (list->vector arr)])
    (define (ptr-narrow left right)
      (let* ([pivot (+ left (quotient (- right left) 2))]
             [pivot-val (vector-ref vec pivot)]
             [lhs-< (< (vector-ref vec (sub1 pivot)) pivot-val)]
             [rhs-< (< (vector-ref vec (add1 pivot)) pivot-val)])
        (cond [(and lhs-< rhs-<) pivot]
              [lhs-< (ptr-narrow (add1 pivot) right)]
              [else (ptr-narrow left (add1 pivot))])))
    (ptr-narrow 0 (sub1 (vector-length vec)))))


(peak-index-in-mountain-array exls)
(peak-index-in-mountain-array exls2)
(peak-index-in-mountain-array '(0 10 5 2))
