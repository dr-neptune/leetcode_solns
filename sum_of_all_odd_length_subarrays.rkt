#lang racket
(require racket)

(define (get-odd-sublengths arr-len)
  (stream->list
   (if (odd? arr-len)
       (in-inclusive-range 1 arr-len 2)
       (in-range 1 arr-len 2))))

(define (sliding-window ls n [vals '()])
  (if (= n (length ls))
      (reverse (cons ls vals))
      (sliding-window (rest ls) n (cons (take ls n) vals))))

(define (subarr-sum arr subarr-length)
  (map (curry apply +) (sliding-window arr subarr-length)))

(define (sum-odd-length-subarrays arr)
  (apply + (flatten
            (map (curry subarr-sum arr)
                 (get-odd-sublengths (length arr))))))

(sum-odd-length-subarrays '(7 6 8 6))
