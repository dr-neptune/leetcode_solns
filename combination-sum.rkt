#lang racket
(require racket)

#|

idea

for each element < target, replicate it until there are more of that element than the target
then take combinations of size 1 -> target size and keep those that sum to target

|#

(let ([candidates '(2 3 6 7)]
      [target 7])
  (let ([cands
         (for/fold ([acc candidates]
                    #:result acc)
                   ([num candidates]
                    #:when (< num target))
           (values (append (make-list (quotient target num) num) acc)))]
        [hsh (make-hash)])

    (for/list ([num-digits (in-range 1 target)])
      (for/list ([cmb (in-combinations cands num-digits)]
                 #:when (equal? target (apply + cmb)))
        cmb))))

(define (unlist lol)
  ())

(define (flatten-depth lst [depth 1])
  "flatten a nested list of lists for `depth` levels"
  (cond [(<= depth 0) lst]
        [(not (list? lst)) (list lst)]
        [else (append-map (lambda (x) (flatten-depth x (sub1 depth))) lst)]))

(let ([candidates '(2 3 6 7)]
      [target 7])
  (let ([cands
         (for/fold ([acc candidates]
                    #:result acc)
                   ([num candidates]
                    #:when (< num target))
           (values (append (make-list (quotient target num) num) acc)))]
        [hsh (make-hash)])
    (for ([num-digits (in-range 1 target)])
      (for ([cmb (in-combinations cands num-digits)]
            #:when (equal? target (apply + cmb)))
        (hash-set! hsh num-digits (cons cmb (hash-ref hsh num-digits '())))))
    (flatten-depth (map (compose remove-duplicates (curry map (curryr sort <))) (hash-values hsh)))))


(define (flatten-depth lst [depth 1])
  "flatten a nested list of lists for `depth` levels"
  (cond [(<= depth 0) lst]
        [(not (list? lst)) (list lst)]
        [else (append-map (lambda (x) (flatten-depth x (sub1 depth))) lst)]))

(define (combination-sum candidates target)
  (let ([cands
         (for/fold ([acc candidates]
                    #:result acc)
                   ([num candidates]
                    #:when (< num target))
           (values (append (make-list (quotient target num) num) acc)))]
        [hsh (make-hash)])
    (for ([num-digits (in-range 1 target)])
      (for ([cmb (in-combinations cands num-digits)]
            #:when (equal? target (apply + cmb)))
        (hash-set! hsh num-digits (cons cmb (hash-ref hsh num-digits '())))))
    (flatten-depth (map (compose remove-duplicates (curry map (curryr sort <))) (hash-values hsh)))))

;; time limit exceeded!


#|
try again
this time, just take values while there is something left
|#
