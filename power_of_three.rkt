#lang racket
(require racket)

;; idea
;; n = 3^x iff log3n = x and x is an integer
(define (count-digits n)
  (+ 1 (floor (/ (log n) (log 10)))))

(define (damping-log n base)
  (let* ([log-result (log n base)]
         [result (inexact->exact (round log-result))])
    (if (< (- (ceiling log-result) log-result)
           (expt 10 (- (+ (count-digits n) 3))))
        result
        log-result)))

(define (is-power-of-three n)
  (and (> n 0)
       (integer? (damping-log n 3))))

(is-power-of-three 243)
(is-power-of-three 45)


;; answer in comments
(define (is-power-of-three n)
  (and (> n 0)
       (= 0 (remainder (expt 3 19) n))))
