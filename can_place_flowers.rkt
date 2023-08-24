#lang racket
(require racket)

;; what a nasty problem

(define (check-available vec)
  (match vec
    [(or (list 1) (list 1 0) (list 0 1)
         (list #\n 1 0) (list #\n 0 1) (list 0 1 #\n) (list 1 0 #\n)
         (list 1 0 0) (list 0 1 0) (list 0 0 1) (list 1 0 1)) #f]
    [(or (list 0) (list 0 0) (list 0 0 0) (list #\n 0 0) (list 0 0 #\n)) #t]))

(define (vrefs vec indices)
  (define (vref vec i)
    (if (or (negative? i)
            (> i (sub1 (vector-length vec))))
        #\n
        (vector-ref vec i)))
  (map (curry vref vec) indices))

(define (can-place-flowers flowerbed n)
  (define flowers (list->vector flowerbed))
  (define flower-length (length flowerbed))
  (define check-flowers
    (λ (flowers n)
      (let loop ([flower-i 0] [n n])
        (cond [(zero? n) #t]
              [(>= flower-i flower-length) #f]
              [else
               (match (vrefs flowers (inclusive-range (sub1 flower-i) (add1 flower-i)))
                 [(? check-available) (loop (+ flower-i 2) (sub1 n))]
                 [_ (loop (add1 flower-i) n)])]))))
  (if (< flower-length 3)
      (match n
        [0 #t]
        [1 (check-available flowerbed)]
        [_ #f])
      (check-flowers flowers n)))

(can-place-flowers (list 1 0 0 0 1 0 0) 2)
(can-place-flowers (list 1 0 0 0 0 1) 2)
(can-place-flowers (list 1 0 0 0 1) 1)
(can-place-flowers (list 1 0 0 0 1) 2)
(can-place-flowers (list 1 0 0 0 1 0 0) 2)
(can-place-flowers (list 1 0) 1)
(can-place-flowers (list 0 0) 2)
(can-place-flowers (list 0 0 0 0) 3)
(can-place-flowers (list 0 0 1 0 1) 1)
