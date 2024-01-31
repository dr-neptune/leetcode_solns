#lang racket
(require racket)


(define (tabulation-fib n)
  (cond [(or (zero? n) (equal? 1 n)) 1]
        [else
         (let ([vals (make-vector n 0)])
           (begin
             (vector-set! vals 0 1)
             (vector-set! vals 1 1)
             (for ([idx (in-range 2 n)])
               (vector-set! vals idx (+ (vector-ref vals (sub1 idx))
                                        (vector-ref vals (- idx 2))))))
           vals)]))


;; (tabulation-fib 8) => #(1 1 2 3 5 8 13 21 34)


(define (memoize-fib n)
  (let ([hsh (make-hash)])
    (define (mem-helper val update)
      (if (hash-has-key? hsh val)
          (hash-ref hsh val)
          (begin
            (hash-set! hsh val update)
            update)))
    (let loop ([idx n])
      (match idx
        [(or 0 1) 1]
        [_
         (let ([fib-val (+ (loop (sub1 idx))
                           (loop (- idx 2)))])
           (mem-helper idx fib-val))]))))

(memoize-fib 8)  ;; => 34

(let ([hsh (make-hash)])
  (define (memoize-fib n)
    (if (or (zero? n) (= 1 n))
        1
        (begin
          (when (not (hash-has-key? hsh n))
            (hash-set! hsh n (+ (memoize-fib (sub1 n))
                                (memoize-fib (- n 2))))
            (displayln (format "n: ~a hsh: ~a" n hsh)))
          (hash-ref hsh n))))
  (memoize-fib 8))  ;; => 34


(define (climb-stairs n)
  (define (dp i)
    ;; base cases
    (if (<= i 2)
        i
        ;; recurrence relation
        (+ (dp (sub1 i)) (dp (- i 2)))))
  (dp n))


(define (climb-stairs n)
  (let ([hsh (make-hash)])
    (define (dp i)
      (if (<= i 2)
          i
          (begin
            (when (not (hash-has-key? hsh i))
              (hash-set! hsh i (+ (dp (sub1 i)) (dp (- i 2)))))
            (hash-ref hsh i))))
    (dp n)))


(climb-stairs 44)


(define (climb-stairs n)
  (when (or (zero? n) (= n 1)) 1)
  (let ([vec (make-vector (add1 n) 0)])
    (vector-set! vec 1 1)
    (vector-set! vec 2 2)
    (for ([i (in-inclusive-range 3 n)])
      (vector-set! vec i (+ (vector-ref vec (sub1 i))
                            (vector-ref vec (- i 2)))))
    (vector-ref vec n)))


(define (climb-stairs n)
  (if (or (zero? n) (= n 1))
      1
      (let ([vec (make-vector n 0)])
        (vector-set! vec 0 1)
        (vector-set! vec 1 2)
        (for ([i (in-range 2 n)])
          (vector-set! vec i (+ (vector-ref vec (sub1 i))
                                (vector-ref vec (- i 2)))))
        (vector-ref vec (sub1 n)))))

(climb-stairs 44)
