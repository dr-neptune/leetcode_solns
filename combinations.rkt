#lang racket
(require racket)

(let ([n 4] [k 2])
  (for/list ([c (in-combinations (stream->list (in-inclusive-range 1 n)) 2)])
    c))

(define (combine n k)
  (combinations (stream->list (in-inclusive-range 1 n)) k))


;; without using combinations

(let ([n 4] [k 2])
  (let ([rng (stream->list (in-inclusive-range 1 n))])
    (remove-duplicates
     (map (curryr sort <)
          (for*/list ([c rng]
                      [d (rest rng)]
                      #:when (not (equal? c d)))
            (list c d))))))


(let ([n 4] [k 2])
  (let ([rng (stream->list (in-inclusive-range 1 n))])

    (remove-duplicates
     (map (curryr sort <)
          (for*/list ([c rng]
                      [d (rest rng)]
                      #:when (not (equal? c d)))
            (list c d))))))

(define inter
  (let loop ([ls '(1 2 3 4 5)] [k 3])
    (displayln (format "~a ~a" k ls))
    (cond [(zero? k) '()]
          [else (let ([shortened (drop ls k)])
                  (cons shortened (loop ls (sub1 k))))])))
