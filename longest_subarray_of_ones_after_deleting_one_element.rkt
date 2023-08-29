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
    [_ (apply max
              (map (λ (3-tup)
                     (+ (first 3-tup) (last 3-tup)))
                   (sliding-window
                    (flatten
                     (map (λ (ls) (if (zero? (first ls))
                                      ls
                                      ((curry apply +) ls))) (partition-binary nums))) 3)))]))

(longest-subarray '(0 1 0))
