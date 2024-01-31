#lang racket
(require racket)

#|

idea

string->list
make a stack
match on each item
if ? then match the last item on the stack
  if T move to the next item
  if F drop until after the :

|#

(let ([expression "F?T:F?T?1:2:F?3:4"])
  (let ([expls (string->list expression)])
    (let loop ([stack '()]
               [exprs expls])
      (displayln (format "exprs: ~a\tstack: ~a" exprs stack))
      (match exprs
        ['() '()]
        [(list a) (list->string exprs)]
        [(list a #\: b _ ...)
         (match (first stack)
           [#\T (list->string (list a))]
           [#\F (list->string (list b))])]
        [(list #\? a) (list->string (rest exprs))]
        [_ (match (first exprs)
             [#\? (match (first stack)
                    [#\T (loop stack (rest exprs))]
                    [#\F (loop stack (rest (dropf exprs (λ (v) (not (equal? v #\:))))))])]
             [_ (loop (cons (first exprs) stack) (rest exprs))])]))))


#|

F?T:F?T?1:2:F?3:4

|#

(define (parse-ternary expression)
  (let ([expls (string->list expression)])
    (let loop ([stack '()]
               [exprs expls])
      (displayln (format "exprs: ~a\tstack: ~a" exprs stack))
      (match exprs
        ['() '()]
        [(list a) (list->string exprs)]
        [(list a #\: b _ ...)
         (match (first stack)
           [#\T (list->string (list a))]
           [#\F (list->string (list b))])]
        [(list #\? a) (list->string (rest exprs))]
        [_ (match (first exprs)
             [#\? (match (first stack)
                    [#\T (loop stack (rest exprs))]
                    [#\F (loop stack (rest (dropf exprs (λ (v) (not (equal? v #\:))))))])]
             [_ (loop (cons (first exprs) stack) (rest exprs))])]))))


;; editorial
(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

(define (parse-ternary expression)
  (define (construct-tree exprls)
    (let ([root ([make-tree-node (first exls)])])
      (for ([expr exprls])
        (match expr
          [#\? ()])
        )))
  (let ([exprls (string->list expression)])
    (let loop ([root root])
      (displayln (format "root: ~a" root))
      (cond [(and (tree-node? (tree-node-left root))
                  (tree-node? (tree-node-right root)))
             (match root
               [(tree-node #\T b c)
                (loop (tree-node-left root))]
               [_ (loop (tree-node-right root))])])
      (tree-node-val root))))

(parse-ternary "T?2:3")
