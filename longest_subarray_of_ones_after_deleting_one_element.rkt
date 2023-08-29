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
  (stream-cons val1 (make-alternating-stream val2 val1)))

(require (only-in srfi/1 map))

(define (longest-subarray nums)
  (let ([1-blocks (filter positive?
                          (map (curry apply +)
                               (let loop ([fn? (make-alternating-stream zero? positive?)]
                                          [lsnums nums]
                                          [acc '()])
                                 (if (null? lsnums)
                                     acc
                                     (let ([tf (takef lsnums (stream-first fn?))]
                                           [df (dropf lsnums (stream-first fn?))])
                                       (loop (stream-rest fn?) df (cons tf acc)))))))])
    (apply max (map + 1-blocks (rest 1-blocks)))))

(longest-subarray exls2)
