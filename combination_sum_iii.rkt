#lang racket
(require racket)

(define exk 3)
(define exn 7)

;; idea
;; brute
;; start with n
;; then subtract 1a
;; then subtract 1b
;; then loop through 1-9c to see if n - 1a - 1b - nc = 0
;; if so, append
;; then add1 to b

(let ([k exk]
      [n exn]
      [results '()])
  (for ([i (in-inclusive-range 1 9)])
    ;; probably some logic here to see if we have below 0
    (for ([j (in-inclusive-range (add1 i) 9)])
      ;; similar
      (for ([k (in-inclusive-range (add1 j) 9)])
        (when (zero? (- n i j k))
          (set! results (cons (list i j k) results)))))
    )
  results)

(define (combination-sum3 k n)
  (let ([results '()])
    (for ([i (in-inclusive-range 1 9)])
      (for ([j (in-inclusive-range (add1 i) 9)])
        (for ([k (in-inclusive-range (add1 j) 9)])
          (when (zero? (- n i j k))
            (set! results (cons (list i j k) results))))))
    results))



(combination-sum3 9 3)
(combination-sum3 9 45)

;; oh neat, should we be doing recursion?
(let ([k 9] [n 45] [results '()])
  (let loop ([k k] [prev-val 1] [inter '()])
    (if (equal? k 0)
        (when (zero? (- n (foldl + 0 inter) prev-val))
          (set! results (cons prev-val inter)))
        (for ([a (in-inclusive-range (add1 prev-val) 9)])
          (loop (sub1 k) (add1 prev-val) (cons a inter)))))
  results)

;; rewrite prev solution recursively
(define (combination-sum3 k n)
  (let ([results '()])
    (for ([i (in-inclusive-range 1 9)])
      (for ([j (in-inclusive-range (add1 i) 9)])
        (for ([k (in-inclusive-range (add1 j) 9)])
          (when (zero? (- n i j k))
            (set! results (cons (list i j k) results))))))
    results))

(let ([k 3] [n 7] [results '()])
  (let loop ([inter '()]
             [prev-val 0])
    (displayln (format "prev: ~a inter: ~a" prev-val inter))
    (if (zero? k)
      (when (zero? (- n (apply + inter) prev-val))
        (set! results (cons prev-val inter)))
      (for ([a (in-inclusive-range (add1 prev-val) 9)])
        (loop (cons a inter) a))))
  results)


(define (combination-sum3 k n)
  (define (helper min used sum)
    (cond
      ((> sum n) '())
      ((= sum n) (if (= (length used) k) (list used) '()))
      ((> (length used) k) '())
      (else
       (append-map (λ(i) (helper (+ i 1) (append used (list i)) (+ sum i)))
                   (range min 10)))))
  (helper 1 '() 0))

(let ([k 3] [n 9])
  (let loop ([])))

(combination-sum3 3 9)
(combination-sum3 9 45)

;; try again

;; idea
;; for k = n
;; get all digits n - k -> 9
;; repeat until k is 0
;; take all combinations of n = 3
;; remove duplicates
(combinations (let loop ([k 3] [n 9])
    (if (zero? k)
        '()
        (cons (stream->list (in-range 1 (- 9 k)))
              (loop (sub1 k) n)))) 3)

(filter (λ (ls) (equal? 7 (apply + ls)))
        (combinations (stream->list (in-inclusive-range 1 (- 7 3))) 3))

(define (combination-sum3 k n)
  (filter (λ (ls) (equal? n (apply + ls)))
          (combinations (stream->list (in-inclusive-range 1 9)) k)))

(define (combination-sum3 k n)
  (sequence->list
   (sequence-filter (λ (ls) (equal? n (apply + ls)))
                    (in-combinations (range 1 10) k))))

(combination-sum3 3 7)
(combination-sum3 3 9)
(combination-sum3 4 1)
(combination-sum3 9 45)
