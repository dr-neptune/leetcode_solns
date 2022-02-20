(require racket)

; val : integer?
; next : (or/c list-node? #f)
(struct list-node
  (val next) #:mutable #:transparent)


; constructor
(define (make-list-node [val 0])
  (list-node val #f))


(define example-linked-list (foldl list-node #f '(1 2 3 4 5)))


;; gets all the values from a linked list in reverse order
;; and places them in a list
(define (get-vals head)
  (if (not (list-node-next head))
      (list (list-node-val head))
      (append (list (list-node-val head))
              (get-vals (list-node-next head)))))

;; builds a "linked list" data structure
(define (reverse-list head)
  (let ([values (get-vals head)])
    (foldl list-node #f values)))


(reverse-list example-linked-list)
