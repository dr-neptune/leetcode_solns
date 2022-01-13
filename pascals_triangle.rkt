#lang racket
(require racket)

;; without memoization
(define (binomial-coef n k [i 1])
    (if (= (add1 k) i)
        1
        (* (/ (- n (- k i)) i)
           (binomial-coef n k (add1 i)))))

(define (binomial n k)
  (if (< (- n k) k)
      (binomial-coef n (- n k))
      (binomial-coef n k)))

(define (generate-row row)
  (map (lambda (k) (binomial row k))
         (range 0 (add1 row))))

(define (generate numRows [k 0])
  (if (= numRows k)
      '()
      (cons (generate-row k)
            (generate numRows (add1 k)))))

;; with memoization (this is actually much slower!)
(define (memoize fn)
  (let ((cache (make-hash)))
    (lambda arg
      (hash-ref! cache arg (thunk (apply fn arg))))))

(define (binomial-coef n k [i 1])
    (if (= (add1 k) i)
        1
        (* (/ (- n (- k i)) i)
           (binomial-coef n k (add1 i)))))

(define binomial
  (memoize
   (lambda (n k)
     (if (< (- n k) k)
         (binomial-coef n (- n k))
         (binomial-coef n k)))))

(define (generate-row row)
  (map (lambda (k) (binomial row k))
         (range 0 (add1 row))))

(define (generate numRows [k 0])
  (if (= numRows k)
      '()
      (cons (generate-row k)
            (generate numRows (add1 k)))))
