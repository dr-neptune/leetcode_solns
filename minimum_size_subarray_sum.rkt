#lang racket
(require racket)

#|

idea

traverse through the array, keeping a sum of the values seen
take a given value:
if curr-sum + given value < target:
add them and continue on
if curr-sum + given value = target
subtract the first value from given sum, add the length to acc of lengths and grab the given value
(i.e. left-drop)
if curr-sum + given value > target:
left-drop

|#

(let ([target 7])
  (let loop ([nums '(2 3 1 2 4 3)]
             [valid '()]
             [curr-sum '()])
    (displayln (format "~a ~a ~a" nums valid curr-sum))
    (cond [(empty? nums) valid]
          [(= (apply + curr-sum) target)
           (loop nums (cons (length curr-sum) valid) (cons (first nums) curr-sum))]
          [(< (apply + curr-sum) target)
           (loop (rest nums) valid (cons (first nums) curr-sum))]
          [else
           (loop nums valid (drop-right curr-sum 1))])))


(let ([target 7])
  (let loop ([nums '(2 3 1 2 4 3)]
             [valid '()]
             [curr-sum '()])
    (displayln (format "~a ~a ~a" nums valid curr-sum))
    (cond [(and (empty? nums)
                (empty? curr-sum)) valid]
          [(= (apply + curr-sum) target)
           (if (empty? nums)
               (loop nums (cons (length curr-sum) valid) curr-sum)
               (loop (rest nums) (cons (length curr-sum) valid) (cons (first nums) curr-sum)))]
          [(< (apply + curr-sum) target)
           (loop (rest nums) valid (cons (first nums) curr-sum))]
          [else
           (loop nums valid (drop-right curr-sum 1))])))


(apply + (cons 4 '(1 2 3)))


#|

(2 3 1 2 4 3)

(2) ()
(3 2) ()
(1 3 2) ()
(2 1 3 2) -> too high -> remove initial val ()
(2 1 3)
(4 2 1 3) -> too high -> remove initial val ()
(4 2 1) -> equals target, add length to valid (3)
(3 4 2 1) -> too high, remove first
(3 4 2) -> too high, remove first
(3 4) -> equals target, add length to valid (2 3)
-> out of elements, return min valid 2

|#
