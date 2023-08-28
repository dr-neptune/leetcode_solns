#lang racket
(require racket)

;; idea
;; iterate through list
;; count max number of 1s in a row
;; if you encounter a 0, subtract 1 from k
;; if you encounter a 0 and k is 0, then see if the current counter is greater than max

(define exls '(1 1 1 0 0 0 1 1 1 1 0))

(define (check-run ls k)
  (let loop ([k k]
             [n ls]
             [curr-run 0]
             [max-run 0])
    (cond [(null? n) (max max-run curr-run)]
          [(> k (length n)) 0]
          [else
           (match (first n)
             [1 (loop k (rest n) (add1 curr-run) max-run)]
             [0 (match k
                  [0 (if (> curr-run max-run)
                         curr-run
                         max-run)]
                  [_ (loop (sub1 k) (rest n) (add1 curr-run) max-run)])])])))

(define (longest-ones nums k)
  (apply max
         (for/list ([i (range (length nums))])
           (let ([window (drop nums i)])
             (check-run window k)))))


(longest-ones exls 2)
(define exls2 '(0 0 0 1))
(longest-ones exls2 4)


;; make it vec
(define (check-run vec k)
  (let loop ([k k]
             [n vec]
             [curr-run 0]
             [max-run 0])
    ;; (displayln (format "~a ~a ~a ~a" k n curr-run max-run))
    (cond [(vector-empty? n) (max max-run curr-run)]
          [(eq? k (vector-length n)) k]
          [else
           (match (vector-ref n 0)
             [1 (loop k (vector-drop n 1) (add1 curr-run) max-run)]
             [0 (match k
                  [0 (if (> curr-run max-run)
                         curr-run
                         max-run)]
                  [_ (loop (sub1 k) (vector-drop n 1) (add1 curr-run) max-run)])])])))

(define (longest-ones nums k)
  (define numvec (list->vector nums))
  (apply max (for/list ([i (range (vector-length numvec))])
               (let ([window (vector-drop numvec i)])
                 (check-run window k)))))

(longest-ones exls 2)
(longest-ones exls2 4)

(define (enumerate ls)
  (map (λ (i v) (cons i v)) (range (length ls)) ls))

(enumerate '(1 2 3 4 5))

;; not done!
