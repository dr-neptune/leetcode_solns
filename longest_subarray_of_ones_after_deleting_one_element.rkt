#lang racket
(require racket)

(define exls '(1 1 0 1))
(define exls2 '(0 1 1 1 0 1 1 0 1))

;; idea
;; ls -> 0 count1 0 count2 etc
;; get the maximal pair and remove the 0 between them

(let loop ([nums exls2]
           [acc 0]
           [final '()])
  (if (null? nums)
      final
      (match (first nums)
        [0 (loop (rest nums) 0 (append final (list acc)))]
        [1 (loop (rest nums) (add1 acc) final)])))

(define (accumulate-sums ls)
  (for/fold ([final '()]
             [acc 0]
             #:result (reverse final))
            ([num (in-list ls)])
    (match num
      [0 (values (cons acc final) 0)]
      [1 (values final (add1 acc))])))

(accumulate-sums exls)


;; better way?
;; split list by 0
(splitf-at exls2 zero?)

(takef (rest exls2) positive?)

;; idea
;; back and forth take positive and zero
(let loop ([nums exls2]
           [fn positive?]
           [acc '()])
  (if (null? nums)
      acc
      (let ([next (takef nums fn)])
        (if (null? next)
            (loop (rest nums) )))))

(for/fold ([fn? (sequence->stream (in-cycle (list zero? positive?)))]
           [acc '()])
          ([num exls2])
  (values fn? acc))

(stream-first (sequence->stream (in-cycle (list zero? positive?))))

(stream->list (stream-take (sequence->stream (in-cycle '(1 0))) 5))


(for/list ([i (in-cycle '(1 0))]
           [j (range 10)])
  i)


(define (make-alternating-stream val1 val2)
  (stream-cons #:eager val1 (make-alternating-stream val2 val1)))

(require (only-in srfi/1 map))

(define (longest-subarray nums)
  (match nums
    [(or (list 1)
         (list 0)
         (list 0 ..2)) 0]
    [(list 1 ...) (sub1 (length nums))]
    [_ (let ([1-blocks (filter positive?
                               (map (curry apply +)
                                    (let loop ([fn? (make-alternating-stream zero? positive?)]
                                               [lsnums nums]
                                               [acc '()])
                                      (if (null? lsnums)
                                          acc
                                          (let ([tf (takef lsnums (stream-first fn?))]
                                                [df (dropf lsnums (stream-first fn?))])
                                            (loop (stream-rest fn?) df (cons tf acc)))))))])
         (map + 1-blocks (rest 1-blocks)))]))


(define (longest-subarray nums)
  (match nums
    [(or (list 1)
         (list 0)
         (list 0 ..2)) 0]
    [(list 1 ...) (sub1 (length nums))]
    [_ (let ([1-blocks (let loop ([fn? (make-alternating-stream zero? positive?)]
                                  [lsnums nums]
                                  [acc '()])
                         (if (null? lsnums)
                             acc
                             (let ([tf (takef lsnums (stream-first fn?))]
                                   [df (dropf lsnums (stream-first fn?))])
                               (loop (stream-rest fn?) df (cons tf acc)))))])
         1-blocks
         ;; (map + 1-blocks (rest 1-blocks))
         )]))

(longest-subarray exls2)
(longest-subarray '(1))
(longest-subarray '(1 1 0 1))
(longest-subarray '(1 1 1))
(longest-subarray '(0 0 0))

(longest-subarray '(1 1 0 0 1 1 1 0 1))

;; not great
;; doesn't account for multiple sets of 0s between sets of 1s

(define exls3 '(1 1 0 0 1 1 1 0 1))

(define (partition-binary ls)
  (define (make-alternating-stream val1 val2)
    (stream-cons val1 (make-alternating-stream val2 val1)))
  (let loop ([fn? (make-alternating-stream zero? positive?)]
             [acc '()]
             [nums exls3])
    (if (null? nums)
        (reverse (filter (compose not empty?) acc))
        (loop (stream-rest fn?)
              (cons (takef nums (stream-first fn?)) acc)
              (dropf nums (stream-first fn?))))))



;; (let loop ([fn? (make-alternating-stream zero? positive?)]
;;            [acc '()]
;;            [nums exls3])
;;   (if (null? nums)
;;       (reverse (filter (compose not empty?) acc))
;;       (loop (stream-rest fn?)
;;             (cons (takef nums (stream-first fn?)) acc)
;;             (dropf nums (stream-first fn?)))))

;; (for/foldr ([fn? (make-alternating-stream zero? positive?)]
;;             [acc '()]
;;             [nums exls3]
;;             #:result
;;             (reverse (filter (compose not empty?) acc)))
;;            ([_ (length exls3)])
;;   (values (stream-rest fn?)
;;           (cons (takef nums (stream-first fn?)) acc)
;;           (dropf nums (stream-first fn?))))


(flatten
 (map (λ (ls) (if (zero? (first ls))
                 ls
                 ((curry apply +) ls))) (partition-binary exls3)))

;; now iterate over the result list and return max (a b c) (+ a c)
;; (2 0 0 3 0 1)
;; (2 + 0) = 2
;; (0 3) = 3
;; (0 0) = 0
;; (3 1) = 4

;; (2 0 1) = 3
;; (0 3 0 2 0 1)
;; (0 0) = 0
;; (3 2) = 5
;; (0 0) = 0
;; (2 1) = 0

(define (partition-binary ls)
  (define (make-alternating-stream val1 val2)
    (stream-cons val1 (make-alternating-stream val2 val1)))
  (let loop ([fn? (make-alternating-stream zero? positive?)]
             [acc '()]
             [nums ls])
    (displayln (format "~a ~a ~a" fn? acc nums))
    (if (null? nums)
        (reverse (filter (compose not empty?) acc))
        (loop (stream-rest fn?)
              (cons (takef nums (stream-first fn?)) acc)
              (dropf nums (stream-first fn?))))))

(define (sliding-window ls n [vals '()])
  (if (= n (length ls))
      (reverse (cons ls vals))
      (sliding-window (rest ls) n (cons (take ls n) vals))))

(define (longest-subarray nums)
  (match nums
    [(or (list 1)
         (list 0)
         (list 0 ..2)) 0]
    [(list 1 ...) (sub1 (length nums))]
    [(list a b c) (sub1 (+ a b c))]
    [_ (apply max
              (map (λ (3-tup)
                     (+ (first 3-tup) (last 3-tup)))
                   (sliding-window
                    (flatten
                     (map (λ (ls) (if (zero? (first ls))
                                      ls
                                      ((curry apply +) ls))) (partition-binary nums))) 3)))]))

(longest-subarray '(0 1 0))


;;

(define (partition-binary ls)
  (define (make-alternating-stream val1 val2)
    (stream-cons val1 (make-alternating-stream val2 val1)))
  (let loop ([fn? (make-alternating-stream zero? positive?)]
             [acc '()]
             [nums ls])
    (if (null? nums)
        (reverse (filter (compose not empty?) acc))
        (loop (stream-rest fn?)
              (cons (takef nums (stream-first fn?)) acc)
              (dropf nums (stream-first fn?))))))

(define (sliding-window ls n [vals '()])
  (if (= n (length ls))
      (reverse (cons ls vals))
      (sliding-window (rest ls) n (cons (take ls n) vals))))

(define (longest-subarray nums)
  (match nums
    [(or (list 1)
         (list 0)
         (list 0 ..2)) 0]
    [(list 1 ...) (sub1 (length nums))]
    [(list a b c) (sub1 (+ a b c))]
    [_ (apply max
       (map (λ (3-tup)
              (+ (first 3-tup) (last 3-tup)))
            (sliding-window
             (flatten
              (map (λ (ls) (if (zero? (first ls))
                               ls
                               ((curry apply +) ls))) (partition-binary nums))) 3)))]))

(longest-subarray '(0 1 0))



;; idea
;; traverse the list
;; have a count of 1s going
;;

(let loop ([nums exls3]
           [max-count 0]
           [zeros-left 1])
  ())

(partition-binary (flatten (remove '(0) (partition-binary exls3))))
(partition-binary (flatten (remove '(0) (partition-binary exls2))))
(partition-binary (flatten (remove '(0) (partition-binary exls))))

;; Hi GPT!
;; I have a list that has an arbitrary number of '(0) elements
;; I want to return a list of lists in which a single '(0) element has been removed
;; but a different one for each element
;; for example: (1 '(0) 1 '(0) 1) -> ((1 1 '(0) 1) (1 '(0) 1 1))
(define (partition-binary ls)
  (define (make-alternating-stream val1 val2)
    (stream-cons val1 (make-alternating-stream val2 val1)))
  (let loop ([fn? (make-alternating-stream zero? positive?)]
             [acc '()]
             [nums ls])
    (if (null? nums)
        (reverse (filter (compose not empty?) acc))
        (loop (stream-rest fn?)
              (cons (takef nums (stream-first fn?)) acc)
              (dropf nums (stream-first fn?))))))

(define (combinations-minus-zero lst)
  (map vector->list (vector->list
                     (let ([vec (list->vector lst)])
                      (for/vector ([i (vector-length vec)]
                                   #:when (equal? (vector-ref vec i) '(0)))
                        (for/vector ([j (vector-length vec)] #:when (not (= i j))) (vector-ref vec j)))))))

(define (longest-subarray nums)
  (match nums
    [(or (list 1)
         (list 0)
         (list 0 ..2)) 0]
    [(list 1 ...) (sub1 (length nums))]
    [_ (apply max
              (map (compose
                    (curry apply max)
                    (curry map (curry apply +))
                    partition-binary
                    flatten)
                   (combinations-minus-zero (partition-binary nums))))]))


(longest-subarray exls)
(longest-subarray exls2)
(longest-subarray '(1 1 1))
(longest-subarray '(0 1 0))
(longest-subarray exls3)
(longest-subarray '(1 0 0 0 0))

(partition-binary '(1 0 0 0 0))


;; try again
;; keep a running tally of 0s left

;; inner loop
;; while num 0s < 1
;; add up number of 1s

(define (get-run ls)
  (for/fold ([current-streak 0]
             [num-zeros 1]
             #:result current-streak)
            ([n ls]
             #:break (negative? num-zeros))
    ;; (displayln (format "~a ~a ~a" current-streak num-zeros n))
    (match n
      [1 (values (add1 current-streak) num-zeros)]
      [0 (values current-streak (sub1 num-zeros))])))

(get-run exls2)
(get-run exls3)
(get-run exls)

;; outer loop
;; start from i = 0
;; move until i = n where n < longest-running


(define (get-run ls)
  (for/fold ([current-streak 0]
             [num-zeros 1]
             #:result current-streak)
            ([n ls]
             #:break (negative? num-zeros))
    ;; (displayln (format "~a ~a ~a" current-streak num-zeros n))
    (match n
      [1 (values (add1 current-streak) num-zeros)]
      [0 (values current-streak (sub1 num-zeros))])))

(define (longest-subarray nums)
  (match nums
    [(list 1 ..1) (sub1 (length nums))]
    [_ length(apply max
         (for/list ([i (range (length nums))])
           (let ([window (drop nums i)])
             (get-run window))))]))

(longest-subarray '(1 1 0 1))
(longest-subarray '(0 1 1 1 0 1 1 0 1))
(longest-subarray '(1 1 1))

(longest-subarray '(1 1 1 1 1 0))




;; redo

;; idea
;; this is a sliding window problem
;; essentially we need to traverse the list and count the max number of 1s in a row
;; if we encounter a 0, we delete it and keep going
;; if we encounter another 0, we reiterate from the start
;; in this case, we have o(n^2)

;; Maintain a sliding window where there is at most one zero on it.

;; (0 1 1 1 0 1 1 0 1)
;; idea
;; get number of 0s
;; start a sliding window across the entire list
;; at start, move through the list until you hit a 0
;; then start removing from the end until you hit a 0
;; then start removing from the front until you hit another 0
;; then the back, etc
;; until there is only 1 0 left

(let loop ([n exls2]
           [num-zeros (- (length exls2) (apply + exls2))])
  (cond [(zero? num-zeros) (apply + n)]
        []))

(takef-right exls2 (λ (v) a))


;; maybe indexes-of can help
;; get indexes of
;; drop values from left and right until there is only 3 left
;; use that to get the sum

(define exls3 '(1 1 1))
(define exls4 '(0 1 1 1 0 1 1))


(let ([idxs (indexes-of exls4 0)]
      [lslen (length exls4)])
  (match idxs
    [(or '() '(a)) (sub1 (length exls4))]
    [(list a b) (max (- (sub1 lslen) (add1 a))
                     (- b (add1 a)))]
    [_ 'pass]))


(let ([idxs (indexes-of exls2 0)]
      [lslen (length exls2)])
  (let loop ([idx idxs])
    (displayln (format "~a" idx))
    (match idx
      [(or '() '(a)) (sub1 (length exls2))]
      [(list a b) (max (- (sub1 lslen) (add1 a))
                       (- b (add1 a)))]
      [_ (apply max (map loop (map cons exls2 (rest exls2))))])))


(require (only-in srfi/1 map))

(map loop (map cons '(0 4 7 9 11) '(4 7 9 11)))

(drop-right exls2 1)

(indexes-of exls 0)
(indexes-of exls4 0)
(indexes-of exls2 0)
