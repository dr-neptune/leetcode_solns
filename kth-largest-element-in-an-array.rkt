#lang racket
(require racket)

(define exls '(3 2 1 5 6 4))
(define exk 2)

;; dumb way first
(define (find-kth-largest nums k)
  (last (take (sort nums >) k)))

;; dumb way works!
;; let's try again with a min-heap
(require data/heap)


(let ([hp (make-heap <=)]
      [k 2])
  (heap-add-all! hp exls)
  (begin
    (for ([_ (- (length exls) k)])
      (heap-remove-min! hp))
    (heap-min hp)))

(require data/heap)

(define (find-kth-largest nums k)
  (let ([hp (make-heap <=)])
    (heap-add-all! hp nums)
    (begin
      (for ([_ (- (length nums) k)])
        (heap-remove-min! hp))
      (heap-min hp))))
