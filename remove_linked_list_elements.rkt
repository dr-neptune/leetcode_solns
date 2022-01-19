#lang racket
(require racket)

; val : integer?
; next : (or/c list-node? #f)
(struct list-node
  (val next) #:mutable #:transparent)

; constructor
(define (make-list-node [val 0])
  (list-node val #f))

(define exll (numbers->linked-list '(1 2 6 3 4 5 6)))
(define exll2 (numbers->linked-list '(7 7 7 7)))

;; beautiful solutions by Oleg Nizhnik
;; https://leetcode.com/problems/remove-linked-list-elements/discuss/1573534/racket-let-tail-recursion-match-simple-and-o1-memory
(define (remove-elements head val)
  (let go ([lst head] [acc null])
    (match lst
           [(list-node h rest) (go rest (if (= h val) acc (cons h acc)))]
           [#f (foldl list-node #f acc)])))


(remove-elements exll 6)
(remove-elements exll2 7)


;; other solutions

;; non-tail recursive solution
(define (remove-elements head val)
  (let go ([lst head])
    (match lst
      [(list-node h rest) #:when (= h val) (go rest)]
      [(list-node h rest) (list-node h (go rest))]
      [#f #f])))

;; with mutability
(define (remove-elements head val)
  (define top (box #f))
  (let go ([lst head] [action! (lambda (n) (set-box! top n))])
    (match lst
      [(list-node h rest) #:when (= h val) (go rest action!)]
      [(list-node _ rest)
       (action! lst)
       (set-list-node-next! lst #f)
       (go rest (lambda (n) (set-list-node-next! lst n)))]
      [#f (void)]))
  (unbox top))

;; fancier version of above with less boilerplate
(define (remove-elements head val)
  (define top (box #f))
  (let go ([lst head] [action! (curry set-box! top)])
    (when lst (match-let
                  ([(list-node h rest) lst]
                   [set-tail! (curry set-list-node-next! lst)])
                (if (= h val)
                    (go rest action!)
                    (begin
                      (action! lst)
                      (set-tail! #f)
                      (go rest set-tail!))))))
  (unbox top))
