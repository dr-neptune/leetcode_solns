#lang racket
(require racket)


;; not sure I understand
;; can I just throw everything into a min heap and
;; pop the min k values, summing them?

(require data/heap)

(define excosts '(17 12 10 2 7 2 11 20 8))
(define exk 3)
(define excandidates 4)

;; think I'm getting it
;; we want to make a min-heap of the first and last candidates costs
;; make 2 min-heaps, 1 for first and 1 for last
;; then show the min from each
;; if first < last or first == last, pop first and append a new val from leftover
;; else pop last and prepend a new val from leftover

(define excosts '(1 2 4 1))
(define exk 3)
(define excandidates 3)

(define excosts '(50 80 34 9 86 20 67 94 65 82 40 79 74 92 84 37 19 16 85 20 79 25 89 55 67 84 3 79 38 16 44 2 54 58))
(define exk 7)
(define excandidates 12)

(remove-right 5 '(1 2 3 4 5 6 7 6 5 4 3 2 1))

(define excosts '(57 33 26 76 14 67 24 90 72 37 30))
(define exk 11)
(define excandidates 2)

(let ([costs excosts]
      [k exk]
      [candidates excandidates])
  (let loop ([costs costs]
             [k k]
             [total-cost 0])
    (displayln (format "costs: ~a \nk: ~a total-cost: ~a\n" costs k total-cost))
    (if (zero? k)
        total-cost
    (let ([lhs (make-heap <=)]
          [rhs (make-heap <=)])
      (begin
        (heap-add-all! lhs (take costs (min candidates (sub1 (length costs)))))
        (heap-add-all! rhs (take-right costs (min candidates (sub1 (length costs)))))
        (let ([lhs-min (heap-min lhs)]
              [rhs-min (heap-min rhs)])
          (displayln (format "lhs: ~a rhs: ~a" lhs-min rhs-min))
          (cond [(or (= lhs-min rhs-min)
                     (< lhs-min rhs-min))
                 (loop (remove lhs-min costs) (sub1 k) (+ total-cost lhs-min))]
                [else
                 (loop (remove-right rhs-min costs) (sub1 k) (+ total-cost rhs-min))])))))))



(define (remove-right v ls)
  (reverse (remove v (reverse ls))))

(define (total-cost costs k candidates)
  (cond [(<= (length costs) k) (foldl + 0 costs)]
        [else
         (let loop ([costs costs]
                    [k k]
                    [total-cost 0])
           (if (zero? k)
               total-cost
               (let ([lhs (make-heap <=)]
                     [rhs (make-heap <=)])
                 (begin
                   (heap-add-all! lhs (take costs (min candidates (sub1 (length costs)))))
                   (heap-add-all! rhs (take-right costs (min candidates (sub1 (length costs)))))
                   (let ([lhs-min (heap-min lhs)]
                         [rhs-min (heap-min rhs)])
                     (cond [(or (= lhs-min rhs-min)
                                (< lhs-min rhs-min))
                            (loop (remove lhs-min costs) (sub1 k) (+ total-cost lhs-min))]
                           [else
                            (loop (remove-right rhs-min costs) (sub1 k) (+ total-cost rhs-min))]))))))]))


;; time limit exceeded
