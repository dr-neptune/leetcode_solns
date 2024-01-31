#lang racket

#|

idea

sort in order of frequency?

make a hash counter and take the top k keys?


|#


(let ([nums '(1 1 1 2 2 3)]
      [k 2])
  (let ([counter (hash-table-counter nums)])
    (map car (take (sort (hash->list counter) #:key cdr >) k))))

(define (hash-table-counter ls)
  (let ([counts (make-hash)])
    (for-each (λ (v) (hash-update! counts v add1 0)) ls)
    counts))

(define (top-k-frequent nums k)
  (let ([counter (hash-table-counter nums)])
    (map car (take (sort (hash->list counter) #:key cdr >) k))))
