(require racket)

;; the stdlib one
(define (contains-duplicate nums)
  (if (check-duplicates nums) #t #f))

;; with a hash table
(define (contains-duplicate nums)
  (let rec ([nums nums] [*hash* (make-hash)])
    (cond [(empty? nums) #f]
          [(hash-ref *hash* (first nums) #f) #t]
          [else (begin (hash-update! *hash* (first nums) add1 0)
                       (rec (rest nums) *hash*))])))


(contains-duplicate '(1 2 3 4 5 1))

(contains-duplicate '(1 2 3 1))

(contains-duplicate '(1 2 3 4))

(contains-duplicate '(1 1 1 3 3 4 3 2 4 2))
