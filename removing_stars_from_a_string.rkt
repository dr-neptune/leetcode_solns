#lang racket
(require racket)

(define exstr "leet**cod*e")

;; idea
;; string->list
;; for each char, add it to a stack
;; if char is a star, discard it and pop the top off the stack
;; after the string is finished, turn it back into a string

(define exstr "erase*****")

(let loop ([sls (string->list exstr)]
           [stack '()])
  (if (null? sls)
      (list->string (reverse stack))
      (cond [(eq? (first sls) #\*)
             (loop (rest sls) (drop stack 1))]
            [else (loop (rest sls) (cons (first sls) stack))])))


(define (remove-stars s)
  (let loop ([sls (string->list s)]
             [stack '()])
    (if (null? sls)
        (list->string (reverse stack))
        (cond [(eq? (first sls) #\*)
               (loop (rest sls) (drop stack 1))]
              [else (loop (rest sls) (cons (first sls) stack))]))))

;; rewrite using a for comp

(for/fold ([stack '()]
           #:result (list->string (reverse stack)))
          ([sls (string->list exstr)])
  (if (eq? sls #\*)
      (values (drop stack 1))
      (values (cons sls stack))))

(define (remove-stars s)
  (for/fold ([stack '()]
             #:result (list->string (reverse stack)))
            ([sls (string->list s)])
    (if (eq? sls #\*)
        (values (drop stack 1))
        (values (cons sls stack)))))
