#lang racket
(require racket)

(struct list-node
  (val next) #:mutable #:transparent)

; constructor
(define (make-list-node [val 0])
  (list-node val #f))

(define (linked-list->numbers linked-list [values '()])
  (cond [(empty? linked-list) '()]
        [(list-node-next linked-list)
         (linked-list->numbers (list-node-next linked-list)
                               (append values (list (list-node-val linked-list))))]
        [else (append values (list (list-node-val linked-list)))]))

(define (numbers->linked-list numbers)
  (define (build-up-list numbers built-up-list)
    (let ([value (make-list-node (first numbers))])
      (if (= 1 (length numbers))
          (begin
            (set-list-node-next! value built-up-list)
            value)
          (begin
            (set-list-node-next! value built-up-list)
            (build-up-list (rest numbers) value)))))
  (cond [(empty? numbers) '()]
        [(= 1 (length numbers)) (make-list-node (first numbers))]
        [else (build-up-list (rest (reverse numbers)) (make-list-node (last numbers)))]))


#|

idea

ll->nums

take m, drop n, take m, drop n, etc

|#

(define exls '(1 2 3 4 5 6 7 8 9 10 11 12 13))
(define exls '(6 3 5 6 2 8 9 2 3 4))
(define exls '(18 10 5 10 7 20 20 17 11 7 5 1 3 10 7 16 14 9 16 4 11))

(define exll (numbers->linked-list exls))
(define exm 2)
(define exn 3)

(let ([head exll] [m exm] [n exn])
  (let ([convert-back (compose numbers->linked-list flatten reverse)])
    (let loop ([num-ls (linked-list->numbers exll)]
               [acc '()] [flip 0])
      (displayln (format "~a ~a ~a" num-ls acc flip))
    (match num-ls
      ['() (convert-back acc)]
      [(list _ ...) #:when (and (< (length num-ls) n) (= 1 flip)) (convert-back acc)]
      [(list _ ...) #:when (and (< (length num-ls) m) (zero? flip)) (cons num-ls acc)]
      [_
       (match flip
         [0 (loop (drop num-ls m) (cons (take num-ls m) acc) 1)]
         [1 (loop (drop num-ls n) acc 0)])]))))



(define (linked-list->numbers linked-list [values '()])
  (cond [(empty? linked-list) '()]
        [(list-node-next linked-list)
         (linked-list->numbers (list-node-next linked-list)
                               (append values (list (list-node-val linked-list))))]
        [else (append values (list (list-node-val linked-list)))]))

(define (numbers->linked-list numbers)
  (define (build-up-list numbers built-up-list)
    (let ([value (make-list-node (first numbers))])
      (if (= 1 (length numbers))
          (begin
            (set-list-node-next! value built-up-list)
            value)
          (begin
            (set-list-node-next! value built-up-list)
            (build-up-list (rest numbers) value)))))
  (cond [(empty? numbers) '()]
        [(= 1 (length numbers)) (make-list-node (first numbers))]
        [else (build-up-list (rest (reverse numbers)) (make-list-node (last numbers)))]))

(define (delete-nodes head m n)
  (let ([convert-back (compose numbers->linked-list flatten reverse)])
    (let loop ([num-ls (linked-list->numbers head)]
               [acc '()] [flip 0])
      (match num-ls
        ['() (convert-back acc)]
        [(list _ ...) #:when (and (< (length num-ls) n) (= 1 flip)) (convert-back acc)]
        [(list _ ...) #:when (and (< (length num-ls) m) (zero? flip)) (convert-back (cons num-ls acc))]
        [_
         (match flip
           [0 (loop (drop num-ls m) (cons (take num-ls m) acc) 1)]
           [1 (loop (drop num-ls n) acc 0)])]))))
