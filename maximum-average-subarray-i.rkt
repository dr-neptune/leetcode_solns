#lang racket
(require racket)

;; 1 pass
;; (1 2 3 4 5) 3
;; use vectors
(require (only-in math/statistics mean))

(define (vrefs vec pos-range)
  (vector-map (curry vector-ref vec) (list->vector pos-range)))

(define (find-max-average nums k)
  (let ([vec (list->vector nums)])
    (let loop ([best 0]
               [sk 0]
               [ek k])
      (cond [(= 1 (vector-length vec)) (vector-ref vec 0)]
            [(> ek (vector-length vec)) best]
            [else
             (let ([curr-mean (mean (vrefs vec (range sk ek)))])
               (if (> curr-mean best)
                   (loop curr-mean (add1 sk) (add1 ek))
                   (loop best (add1 sk) (add1 ek))))]))))


(find-max-average exls 4)
(find-max-average '(5) 1)
(find-max-average '(-1) 1)


(define (find-max-average nums k)
  (let loop ([i 0]
             [M 0])
    (if (= i (- (length nums) k))
        (/ (+ (apply + (take nums k)) M) k)
        (let ([d (- (list-ref nums (+ i k)) (list-ref nums i))])
          (if (> d M)
              (loop (add1 i) d)
              (loop (add1 i) M))))))


;; try again

(define exls '(1 12 -5 -6 50 3))


(define exls2 '(4 2 1 3 3))

(define (find-max-average nums k)
  (let loop ([i 0]
             [M 0])
    (displayln (format "~a ~a" i M))
    (if (= i (- (length nums) k))
        (/ (+ (apply + (take nums k)) M) k)
        (let ([d (- (list-ref nums (+ i k)) (list-ref nums i))])
          (if (> d M)
              (loop (add1 i) d)
              (loop (add1 i) M))))))

(find-max-average exls2 2)

(define (sliding-window ls n [vals '()])
  (if (= n (length ls))
      (reverse (cons ls vals))
      (sliding-window (rest ls) n (cons (take ls n) vals))))

;; idea
;; sliding window of length k
;; get the average for each window and return the max
(require (only-in math mean))

(define (vrefs vec pos-range)
  (vector-map (curry vector-ref vec) (list->vector pos-range)))

(define (sliding-window ls n)
  (define vec (list->vector ls))
  (for/vector ([i (add1 (- (vector-length vec) n))])
    (vrefs vec (range i (+ i n)))))

(define (find-max-average nums k)
  (sequence-fold max -10000 (vector-map  (sliding-window nums k))))
sliding-window
(find-max-average '(-1) 1)

(mean '(-1))


;; idea
;; go over all the values
(require (only-in math mean))

(define (sliding lst size [step 1])
  (define (tail-call lst)
    (if (>= size (length lst))
        (list lst)
        (cons (take lst size)
              (tail-call (drop lst step)))))
  (if (>= step (length lst))
      (error "step has to be smaller then length of the list")
      (tail-call lst)))

(define (find-max-average nums k)
  (match (length nums)
    [1 (first nums)]
    [_ (apply max (map mean (sliding nums k)))]))

;; how frustrating
;; try single pass

;; we want to grab the sums of elements in a window size of 3
;; we want to keep track of the max and return it divided by the window size

(length '())

(define (find-max-average nums k [max-val (* -10000 k)])
  (match (length nums)
    [(? (curry > k)) (/ max-val k)]
    [_ (let ([window-sum (apply + (take nums k))])
         (if (> window-sum max-val)
             (find-max-average (rest nums) k window-sum)
             (find-max-average (rest nums) k max-val)))]))
>
(find-max-average exls 4)
(find-max-average '(-1) 1)


(vector-take #(1 2 3 4 5) 2)


;; try with a for loop
(define (find-max-average nums k)
  (for/fold ([max-val (* -10000 k)]
             []
             [window-length k])
            ([i (- (length nums) 3)])
    (if (zero? window-length)
        (if (> )))))


(define (find-max-average nums k [max-val (* -10000 k)])
  (define (helper remaining-window-sum remaining-nums max-val)
    (cond
      [(< (length remaining-nums) k) (/ max-val k)]
      [else
       (let* ([window-sum (+ remaining-window-sum (first (drop remaining-nums (- k 1))))] ; calculate the new window sum by adding the next number and subtracting the first number in the current window
              [new-max (if (> window-sum max-val) window-sum max-val)]) ; update the max value if needed
         (helper window-sum (rest remaining-nums) new-max))]))

  (helper (apply + (take nums k)) (rest nums) max-val))


;; try again
;; don't repeat calculations

(let loop ([nums exls]
           [fval (first exls)]
           [max-val (list initial)])
  (cond [(null? nums) acc]
        [else (loop (rest nums)
                    ()
                    (+ fval ))]))

(define vec (list->vector exls))

(let loop ([v vec]
           []))

;; initialization
;; we want to get the sum of the first k values and return the first val and the sum of the first k
;; then onwards we want to subtract the first val, increment it, and add the next val

(define (find-max-average nums k)
  (if (= k (length nums))
      (/ (apply + nums) k)
      (let loop ([fval nums]
                 [rvals (drop nums k)]
                 [prev-val (apply + (take nums k))]
                 [max-val (apply + (take nums k))])
        (match rvals
          ['() (/ max-val k)]
          [_ (let ([window-sum (+ (- prev-val (first fval)) (first rvals))])
               (if (> window-sum max-val)
                   (loop (rest fval) (rest rvals) window-sum window-sum)
                   (loop (rest fval) (rest rvals) window-sum max-val)))]))))


;; final version
;; after much struggle
;; the problem needs a single pass otherwise you get a time out

(define (find-max-average nums k)
  (define initial-sum (apply + (take nums k)))
  (let loop ([first-vals nums]
             [rest-vals (drop nums k)]
             [prev-val initial-sum]
             [max-val initial-sum])
    (match rest-vals
      ['() (/ max-val k)]
      [_ (let ([window-sum (+ (- prev-val (first first-vals)) (first rest-vals))])
           (if (> window-sum max-val)
               (loop (rest first-vals) (rest rest-vals) window-sum window-sum)
               (loop (rest first-vals) (rest rest-vals) window-sum max-val)))])))

(find-max-average '(5) 1)
(find-max-average '(4 0 4 3 3) 5)
(find-max-average '(0 4 0 3 2) 1)
(find-max-average exls 4)
(find-max-average '(4 2 1 3 3) 2)
(find-max-average '(9 7 3 5 6 2 0 8 1 9) 6)
