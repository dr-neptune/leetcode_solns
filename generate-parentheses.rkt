#lang racket
(require racket)

#|

idea

          3      2     2 1   1 2    1 1 1

output: ((())) (()()) (())() ()(()) ()()()

|#


(define (valid? pattern)
  (for/fold ([stack '()]
             #:result (empty? stack))
            ([letter pattern]
             #:break (and (not (empty? stack))
                          (false? (first stack))))
    (match letter
      ['L (values (cons letter stack))]
      ['R (cond [(empty? stack) (cons #f stack)]
                [(equal? (first stack) 'L) (rest stack)]
                [else (cons #f stack)])])))


(define (replace-vals pattern)
  (for/list ([dir pattern])
    (match dir
      ['L #\(]
      ['R #\)])))

(define (generate-parenthesis n)
  (remove-duplicates
   (stream->list
    (stream-map (compose list->string replace-vals)
                (stream-filter valid? (sequence->stream (in-permutations (append (make-list n 'L) (make-list n 'R)))))))))

;; seems to work, but times out

;; divide and conquer
#|

    def generateParenthesis(self, n: int) -> List[str]:
        if n == 0:
            return [""]

        answer = []
        for left_count in range(n):
            for left_string in self.generateParenthesis(left_count):
                for right_string in self.generateParenthesis(n - 1 - left_count):
                    answer.append("(" + left_string + ")" + right_string)

        return answer

|#

(define (generate-parenthesis n)
  (if (zero? n)
      '("")
      (let ([answer '()])
        (for ([left-count (in-range n)])
          (for ([left-string (in-list (generate-parenthesis left-count))])
            (for ([right-string (in-list (generate-parenthesis (- n 1 left-count)))])
              (set! answer (cons (format "(~a)~a" left-string right-string) answer)))))
        answer)))

(generate-parenthesis 6)

(define (generate-parenthesis n)
  (set->list
   (let loop ([l 0] [r 0] [p '()] [a (set)])
     (match (list l r)
       [(list _ (== n)) (set-add a (list->string (reverse p)))]
       [(list (== n) _) (loop l (add1 r) (cons #\) p) a)]
       [(list a b) #:when (< r l)
                   (let ([addl (cons #\( p)]
                         [addr (cons #\) p)])
                     (set-union (loop (add1 l) r addl a)
                                (loop l (add1 r) addr a)))]
       [_ (loop (add1 l) r (cons #\( p) a)]))))
