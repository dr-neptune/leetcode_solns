#lang racket
(require racket
         (only-in math/matrix matrix matrix-num-cols matrix-trace matrix-ref list->matrix))

(define (ls->mat ls)
  (let ([side-len (length ls)])
    (list->matrix side-len side-len (flatten ls))))

(define (get-odd-center-element m)
  (if (odd? (matrix-num-cols m))
      (let ([mid (floor (/ (matrix-num-cols m) 2))])
        (matrix-ref m mid mid))
      0))

(define (diagonal-sum mat)
  (if (= (length mat) 1)
      (caar mat)
      (let ([mat (ls->mat mat)]
            [rev-mat (ls->mat (map reverse mat))])
        (- (+ (matrix-trace mat)
              (matrix-trace rev-mat))
           (get-odd-center-element mat)))))

(define even-mat (matrix ([1 2][3 4])))
(define odd-mat (matrix ([1 2 3][4 5 6][7 8 9])))

(diagonal-sum odd-mat)
(diagonal-sum even-mat)
(diagonal-sum (matrix ([1 1 1 1][1 1 1 1][1 1 1 1][1 1 1 1])))
