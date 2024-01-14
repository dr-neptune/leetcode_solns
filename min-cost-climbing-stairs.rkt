#lang racket
(require racket)

;; idea
;; start from the back?
(define exls1 '(10 15 20))
(define exls '(1 100 1 1 1 100 1 1 100 1))

;; 1 step -> 1
;; 2 steps -> 1
;; 1 step -> 1
;; 2 steps -> 1
;; 2 steps -> 1
;; 2 steps -> 1
;; total cost: 6

(let ([cost exls])
  (let loop ([costs cost]
             [paid '()])
    (match costs
      [(list a b c ..1)
       (let-values ([(rest-costs candidates) (split-at-right costs 2)])
         (displayln (format "candidates: ~a rest-costs: ~a" candidates rest-costs))
         (loop rest-costs (cons (apply min candidates) paid)))]
      [(list a b) (cons (min a b) paid)])))

;; if we take the first, we must only drop 1
;; otherwise we must drop 2
(let ([cost exls])
  (apply + (let loop ([costs cost]
             [paid '()])
    (match costs
      [(list a b c ..1)
       (let-values ([(rest-costs candidates) (split-at-right costs 2)])
         (displayln (format "candidates: ~a rest-costs: ~a" candidates rest-costs))
         (match candidates
           [(list a b) #:when (> a b) (loop (append rest-costs (list a)) (cons b paid))]
           [(list a _) (loop rest-costs (cons a paid))]))]
      [(list a b) (cons (min a b) paid)]
      [(list a) paid]))))

(define (min-cost-climbing-stairs cost)
  (apply +
         (let loop ([costs cost]
                    [paid '()])
           (match costs
             [(list a b c ..1)
              (let-values ([(rest-costs candidates) (split-at-right costs 2)])
                ;; (displayln (format "candidates: ~a rest-costs: ~a" candidates rest-costs))
                (match candidates
                  [(list a b) #:when (> a b) (loop (append rest-costs (list a)) (cons b paid))]
                  [(list a _) (loop rest-costs (cons a paid))]))]
             [(list a b) (cons (min a b) paid)]
             [(list a) paid]))))

(define exls2 '(0 2 2 1))

(let ([cost exls2])
  (apply + (let loop ([costs cost]
             [paid '()])
    (match costs
      [(list a b c ..1)
       (let-values ([(rest-costs candidates) (split-at-right costs 2)])
         (displayln (format "candidates: ~a rest-costs: ~a paid: ~a" candidates rest-costs paid))
         (match candidates
           [(list a b) #:when (> a b) (loop (append rest-costs (list a)) (cons b paid))]
           [(list a _) (loop rest-costs (cons a paid))]))]
      [(list a b) (cons (min a b) paid)]
      [(list a) paid]))))

;; this is a greedy approach
;; but it is not globally optimal

;; idea
;; at each step, we should split depending on whether we take 1 or 2 values
;; then we can build up a set of possible traversals
;; and take the minimum

(let ([cost exls2])
  (let loop ([costs cost]
             [paid '()])
    (match costs
      [(list a b c ...)
       (let-values ([(rest-costs candidates) (split-at-right costs 2)])
         (displayln (format "candidates: ~a rest-costs: ~a paid: ~a" candidates rest-costs paid))
         (let ([a (first candidates)]
               [b (second candidates)])
          (append (loop rest-costs (cons a paid))
                  (loop (append rest-costs (list a)) (cons b paid)))))]
      [_ (list paid)])))




;; logic above
;; if we have more than 2 in list
;; take back 2
;; then we want to get the results for both if we take a or if we take b
(let ([cost exls2])
  (let loop ([costs cost]
             [paid '()])
    (match costs
      [(list a b c ...)
       (let-values ([(rest-costs candidates) (split-at-right costs 2)])
         (displayln (format "candidates: ~a rest-costs: ~a paid: ~a" candidates rest-costs paid))
         (let ([a (first candidates)]
               [b (second candidates)])
          (append (loop rest-costs (cons a paid))
                  (loop (append rest-costs (list a)) (cons b paid)))))]
      [_ (list paid)])))


;; alternative
;; it will likely never be sets of 1
;; what if we got all partitions of min-jumps -> len
;; where min-jumps is len quotient 2?

(map (curry apply min)
     (map (λ (set)
            (map (curry apply +) set))
          (let* ([cost exls2]
                 [min-jumps (quotient (length exls2) 2)])
            (map (curry combinations cost) (range min-jumps (length cost))))))

;; maybe we could stream combinations
;; and break the sampling if a later set min is higher than a previous set



;; logic above
;; if we have more than 2 in list
;; take back 2
;; then we want to get the results for both if we take a or if we take b
(apply min (let ([cost exls])
  (map (curry apply +)
       (let loop ([costs cost]
             [paid '()])
    (match costs
      [(list a b c ...)
       (let-values ([(rest-costs candidates) (split-at-right costs 2)])
         (displayln (format "candidates: ~a rest-costs: ~a paid: ~a" candidates rest-costs paid))
         (let ([a (first candidates)]
               [b (second candidates)])
          (append (loop rest-costs (cons a paid))
                  (loop (append rest-costs (list a)) (cons b paid)))))]
      [_ (list paid)])))))

(define (min-cost-climbing-stairs cost)
  (apply min
         (map (curry apply +)
              (let loop ([costs cost]
                         [paid '()])
                (match costs
                  [(list a b c ...)
                   (let-values ([(rest-costs candidates) (split-at-right costs 2)])
                     (match candidates
                       [(list a b)
                        (append (loop rest-costs (cons a paid))
                               (loop (append rest-costs (list a)) (cons b paid)))]))]
                  [_ (list paid)])))))

;; memory limit exceeded!
(min-cost-climbing-stairs exls)
(min-cost-climbing-stairs exls1)
(min-cost-climbing-stairs exls2)


(let ([cost exls])
  (apply min
         (map (curry apply +)
              (let loop ([costs cost]
                         [paid '()])
                (match costs
                  [(list a b c ...)
                   (let-values ([(rest-costs candidates) (split-at-right costs 2)])
                     (match candidates
                       [(list a b)
                        (append (loop rest-costs (cons a paid))
                                (loop (append rest-costs (list a)) (cons b paid)))]))]
                  [_ (list paid)])))))

;; we can fix this with memoization
;; or we can just add the values lol

(let ([cost exls2])

  )


(define (min-cost-climbing-stairs cost)
  (let ([paths (let loop ([costs cost]
                    [paid 0])
           (match costs
             [(list a b c ...)
              (let-values ([(rest-costs candidates) (split-at-right costs 2)])
                (match candidates
                  [(list a b)
                   (append (loop rest-costs (+ a paid))
                           (loop (append rest-costs (list a)) (+ b paid)))]))]
             [_ (list paid)]))])
    (apply min paths)))


;; with soln
(let ([cost exls])
  (let ([n (length cost)]
        [dp (make-hash)])
    (define (min-cost n)
      (displayln (format "~a ~a" n dp))
      (match n
        [(? negative?) 0]
        [(or 0 1) (list-ref cost n)]
        [(? (not (zero? (hash-ref dp n 0)))) (hash-ref dp n 0)]
        [_ (begin
             (hash-set! dp n (+ (list-ref cost n)
                                (min (min-cost (sub1 n))
                                     (min-cost (sub1 (sub1 n))))))
             (hash-ref dp n))]))
    (min (min-cost (sub1 n)) (min-cost (sub1 (sub1 n))))))


;; try again
;; (1 100 1 1 1 100 1 1 100 1).
;; we want to keep track of our current step
;; and our previous 2 steps
;; for our step, we want to minimize by taking either (+ n n-1) or (+ n n-2)
(let ([cost exls])
  (for/fold ([n-2 0]
             [n-1 0]
             #:result (min n-1 n-2))
            ([n cost])
    (values n-1 (min (+ n n-1) (+ n n-2)))))

;; start at index 0
;; (1 100 1 1 1 100 1 1 100 1)
;;

(define (min-cost-climbing-stairs cost)
  (for/fold ([n-2 0]
             [n-1 0]
             #:result (min n-1 n-2))
            ([n cost])
    (values n-1 (min (+ n n-1) (+ n n-2)))))


;; for dp explore

;; recursive case
;; dp(i) = (min (+ cost dp(i - 1)), (+ cost dp(i - 2)))

;; base case
;; 1 step -> step cost
;; 2 steps -> min(step1, step2)

;; go down steps backwards

(let ([cost '(1 100 1 1 1 100 1 1 100 1)])
  (define hsh (make-hash))
  (define (dp i)
    (match i
      [0 (list-ref cost i)]
      [1 (min (list-ref cost 0)
              (list-ref cost 1))]
      [_
       (begin
         (when (not (hash-has-key? hsh i))
           (hash-set! hsh i (min (+ (list-ref cost i) (dp (sub1 i))) (+ (list-ref cost i) (dp (- i 2))))))
         (hash-ref hsh i))]))
  (dp (sub1 (length cost))))


(define (min-cost-climbing-stairs cost)
  (define hsh (make-hash))
  (define (dp i)
    (match i
      [0 (list-ref cost i)]
      [1 (min (list-ref cost 0)
              (list-ref cost 1))]
      [_
       (begin
         (when (not (hash-has-key? hsh i))
           (hash-set! hsh i (min (+ (list-ref cost i) (dp (sub1 i))) (+ (list-ref cost i) (dp (- i 2))))))
         (hash-ref hsh i))]))
  (dp (sub1 (length cost))))
;; lol, leetcode is down
