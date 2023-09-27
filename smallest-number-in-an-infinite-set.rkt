#lang racket
(require racket data/heap)

(define smallest-infinite-set%
  (class object%
    (super-new)

    (init-field [hp (make-heap <=)] [curr-smallest 0])

    ; pop-smallest : -> exact-integer?
    (define/public (pop-smallest)
      ;; if the heap is empty, we want
      ;; to add move values
      ;; furthermore, the values we add should
      ;; be in line with what we've seen
      ;; we also need to add the logic that looks at what we've seen before
      (let ([ele (heap-min hp)])
        (begin
          (heap-remove-min! hp)
          (set! curr-smallest ele)
          (when (> ele curr-smallest)
            (set! curr-smallest ele))
          (add-back (add1 curr-smallest))
          ele)))

    ; add-back : exact-integer? -> void?
    (define/public (add-back num)
      (begin
        (heap-remove-eq! hp num)
        (heap-add! hp num)))))

;; maybe heap-remove-eq! to remove duplicates
;; and heap-remove-min! to pop

;; Your smallest-infinite-set% object will be instantiated and called as such:
(define obj (new smallest-infinite-set%))
(define param_1 (send obj pop-smallest))
(send obj add-back 1)

(for ([i (in-range 100)])
  (send obj add-back i))

(define obj (new smallest-infinite-set%))
(send obj pop-smallest)
(send obj add-back 0)




;; maybe we can do heap-remove-eq! and then add

(let ([hp (make-heap <=)]
      [ls '(1 2 4 5)])
  (begin
    (heap-add-all! hp ls)
    (heap-remove-eq! hp 3)
    (for ([x (in-heap/consume! hp)])
      (displayln x))))

(require data/heap)

(define smallest-infinite-set%
  (class object%
    (super-new)

    (init-field [hp (make-heap <=)]
                [curr-smallest 0])

    (heap-add! hp 1)

    ; pop-smallest : -> exact-integer?
    (define/public (pop-smallest)
      (let ([ele (heap-min hp)])
        (begin
          (heap-remove-min! hp)
          (when (> ele curr-smallest)
            (set! curr-smallest ele))
          (add-back (add1 curr-smallest))
          ele)))

    ; add-back : exact-integer? -> void?
    (define/public (add-back num)
      (heap-add! hp num)
      (begin
        (heap-remove-eq! hp num)
        (heap-add! hp num)))))


(define obj (new smallest-infinite-set%))
(send obj pop-smallest)
(send obj add-back 100)
