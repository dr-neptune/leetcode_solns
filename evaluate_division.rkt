#lang racket
(require racket)

(define equations '(("a" "b") ("b" "c")))
(define vals '(2.0 3.0))
(define queries '(("a" "c") ("b" "a") ("a" "e") ("a" "a") ("x" "x")))

(define equations2 '(("a" "b") ("b" "c") ("bc" "cd")))
(define vals2 '(1.5 2.5 5.0))
(define queries2 '(("a" "c") ("c" "b") ("bc" "cd") ("cd" "bc")))

(define equations3 '(("a" "e") ("b" "e")))
(define vals3 '(4.0 3.0))
(define queries3 '(("a" "b") ("e" "e") ("x" "x")))


;; a/b = 2 -> a = 2b
;; b/c = 3 -> b = 3c
;; => a = 6c
;; => c = a / 6

;; a/c = a / (a / 6) = 6
;; b/a = b/2b = 1/2
;; a/e = -1 since e is undefined
;; a/a = 1
;; x/x = -1 since x is undefined

;; idea
;; make a list of valid expressions
;; if a query contains an item that isn't in the list of valid expressions
;; return -1

;; (define (valid? pair valid-vars)
;;   ((compose not false?)
;;    (andmap ((curryr member valid-vars)) pair)))

;; replace with something more specialized
(define (valid? pair valid-pairs)
  (match pair
    [(list a a) 1]
    [(list a b)
     #:when (and (member a valid-pairs)
                 (member b valid-pairs)) pair]
    [_ -1]))

(define (valid? pair valid-pairs)
  (let ([valid-vars (andmap (curryr member valid-pairs) pair)])
    (match pair
      [(list a a) #:when valid-vars 1]
      [(list a b) #:when valid-vars pair]
      [_ -1])))

(valid? '("a" "c") '("a" "b" "c" "d"))
(valid? '("a" "e") '("a" "b" "c" "d"))
(valid? '("a" "a") '("a" "b" "c" "d"))
(valid? '("x" "x") '("a" "b" "c" "d"))

(let ([equations equations]
      [values vals]
      [queries queries]
      [valid-variables (remove-duplicates (flatten equations))])
  (map (curryr valid? valid-variables) queries))

;; ok, now we need to do the reduction step

(= (/ a b) 2)
(= (/ b c) 3)

;; maybe eval a single item to a digit
;; and then divide by it later

;; ex
;; a/b = 2
;; let a = 1
;; then 2b = 1 -> b = 1/2
;; b/c = 3 -> (1/2)/c = 3
;; => 3c = 1/2 -> c = 1/6

;; ex2
;; (/ a b) = 1.5
;; (/ b c) = 2.5
;; (/ bc cd) = 5.0
;; a = 1.5b
;; b = 2.5c
;; bc = 5*cd
;; a = 1.5(2.5c) = 3/2 * 5/2 c = 15/2 c
;; (/ a c) = (/ 15/2*c c)
;; (/ c b) =

;; we need to get the numerator in terms of the denominator
;; then we can cancel both and just do the coefficient

;; lets try with example 1
(let ([query '("a" "c")]
      [equations equations]
      [values vals])
  ;; now get a in terms of c
  (let loop ([equations equations]
             [values vals])
    )
  )

;; (a b) (b c)
;; (2 3)
;; (2 b) (b c)
;; (2 (b c))
;; (2 3 c)
;; (6 c)

;; (a b) (b c)
;; (1.5 2.5)
;; (1.5 b) (b c)
;; (1.5 (b c))
;; (1.5 (2.5 c))
;; (3.75 c)

(define (get-letter letter pairs)
  (findf (λ (pair) (equal? (first (car pair)) letter)) pairs))

(let ([pairs (map cons equations vals)]
      [goal-letter "c"])
  (foldl * 1
         (drop-right
          (let loop ([pair (get-letter "a" pairs)])
            (let ([vars (car pair)]
                  [coef (cdr pair)])
              (displayln (format "pair: ~a vars: ~a coef: ~a" pair vars coef))
              (if (equal? (second vars) goal-letter)
                  (cons coef goal-letter)
                  (cons coef
                        (loop (get-letter (second vars) pairs)))))) 0)))


(define (reduction pairs start-letter goal-letter)
  (define (get-letter letter pairs)
    (findf (λ (pair) (equal? (first (car pair)) letter)) pairs))
  (foldl * 1
         (drop-right
          (let loop ([pair (get-letter start-letter pairs)])
            (let ([vars (car pair)]
                  [coef (cdr pair)])
              (displayln (format "pair: ~a vars: ~a coef: ~a" pair vars coef))
              (if (equal? (second vars) goal-letter)
                  (cons coef goal-letter)
                  (cons coef
                        (loop (get-letter (second vars) pairs)))))) 0)))

(with-handlers ([exn:fail? (λ (exn)
                             (reduction (map cons (map reverse equations) (map (λ (v) (/ 1 v)) vals)) "b" "a"))])
  (reduction (map cons equations vals) "b" "a"))



(reduction (map cons equations vals) "a" "c")
(reduction (map cons equations vals) "a" "b")
(reduction (map cons equations vals) "b" "a")
(reduction (map cons (map reverse equations) (map (λ (v) (/ 1 v)) vals)) "b" "a")


;; now for whatever is valid, we want to do reduction



(let* ([equations equations2]
       [values vals2]
       [queries queries2]
       [valid-variables (remove-duplicates (flatten equations))]
       [coef-pairs (map cons equations values)])
  (let ([inter (map (curryr valid? valid-variables) queries)])
    (map (λ (v) (if (list? v)
                    (begin
                      (println v)
                      (with-handlers ([exn:fail? (λ (exn)
                                                   (reduction
                                                    (map (λ (v) (cons (reverse (car v)) (/ 1 (cdr v)))) coef-pairs)
                                                    (first v) (second v)))])
                        (reduction coef-pairs (first v) (second v))))
                    v))
         inter)))

;; we need to be able to handle backwards search
;; (/ a b) = 2
;; -> a = 2b -> b = 1/2 a
;; (/ b c) = 3
;; we can swap the direction by flipping the vars and adding a 1 over the coef

;; wrap it all up!
(define (valid? pair valid-pairs)
  (let ([valid-vars (andmap (curryr member valid-pairs) pair)])
    (match pair
      [(list a a) #:when valid-vars 1]
      [(list a b) #:when valid-vars pair]
      [_ -1])))

(define (get-letter letter pairs)
  (findf (λ (pair) (equal? (first (car pair)) letter)) pairs))

(define (reduction pairs start-letter goal-letter)
  (define (get-letter letter pairs)
    (findf (λ (pair) (equal? (first (car pair)) letter)) pairs))
  (foldl * 1
         (drop-right
          (let loop ([pair (get-letter start-letter pairs)])
            (let ([vars (car pair)]
                  [coef (cdr pair)])
              (displayln (format "pair: ~a vars: ~a coef: ~a" pair vars coef))
              (if (equal? (second vars) goal-letter)
                  (cons coef goal-letter)
                  (cons coef
                        (loop (get-letter (second vars) pairs)))))) 0)))

(define (calc-equation equations values queries)
  (let* ([valid-variables (remove-duplicates (flatten equations))]
         [coef-pairs (map cons equations values)])
    (let ([inter (map (curryr valid? valid-variables) queries)])
      (map (λ (v) (if (list? v)
                      (begin
                        (println v)
                        (with-handlers ([exn:fail? (λ (exn)
                                                     (reduction
                                                      (map (λ (v) (cons (reverse (car v)) (/ 1 (cdr v)))) coef-pairs)
                                                      (first v) (second v)))])
                          (reduction coef-pairs (first v) (second v))))
                      v))
           inter))))

(calc-equation equations3 vals3 queries3)

;; (a e) (b e)
;; (4 3)
;; (/ a e) = 4
;; a = 4e
;; (/ b e) = 3
;; b = 3e
;; (/ a b) = (/ 4e 3e)

;; maybe we can dynamically find our way
;; (a e) (b e)
;; (4 3)
;; (/ a e) = 4
;; a = 4 e
;; first e not found
;; flip it
;; (a e) (e b)
;; (4 1/3)
;; a = 4 e
;; (/ e b) = 1/3
;; (4 1/3 b)
;; 4/3

;; this requires rewriting reduction and changing the exception handler in calc-equation

(define (reduction pairs start-letter goal-letter)
  (define (get-letter letter pairs)
    (let ([search (findf (λ (pair) (equal? (first (car pair)) letter)) pairs)])
      (if search
          search
          (findf (λ (pair) (and (equal? (second (car pair)) letter)
                                (not (equal? (first (car pair)) letter)))) (rest pairs)))))
  (foldl * 1
         (drop-right
          (let loop ([pair (get-letter start-letter pairs)])
            (let ([vars (car pair)]
                  [coef (cdr pair)])
              (displayln (format "pair: ~a vars: ~a coef: ~a" pair vars coef))
              (if (equal? (second vars) goal-letter)
                  (cons coef goal-letter)
                  (cons coef
                        (loop (get-letter (second vars) pairs)))))) 0)))



(let ([coef-pairs (map cons equations3 vals3)])
  ;;coef-pairs
  (define (get-letter letter pairs)
    (let ([search (findf (λ (pair) (equal? (first (car pair)) letter)) pairs)])
      (if search
          search
          (findf (λ (pair) (and (equal? (second (car pair)) letter)
                                (not (equal? (first (car pair)) letter)))) (rest pairs)))))
  (get-letter "e" coef-pairs))

;; if we don't find the letter, mutate the pairs?
;; to be finished!
